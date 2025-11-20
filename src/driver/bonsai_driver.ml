open! Core
module Incr = Ui_incr
module Stabilization_tracker = Bonsai.Private.Stabilization_tracker
module Action = Bonsai.Private.Action
module Instrumentation = Instrumentation
module Timer = Bonsai.Private.Timer

type ('m, 'action, 'action_input, 'r, 'timer) unpacked =
  { model_var : 'm Incr.Var.t
  ; default_model : 'm
  ; time_source : Bonsai.Time_source.t
  ; inject : 'action Action.t -> unit Ui_effect.t
  ; sexp_of_model : 'm -> Sexp.t
  ; sexp_of_action : 'action Action.t -> Sexp.t
  ; action_input_incr : 'action_input Incr.t
  ; action_input : 'action_input Incr.Observer.t
  ; apply_action :
      schedule_event:(unit Ui_effect.t -> unit)
      -> 'action_input option
      -> 'm
      -> 'action Action.t
      -> 'm
  ; result : 'r Incr.Observer.t
  ; result_incr : 'r Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; lifecycle_incr : Bonsai.Private.Lifecycle.Collection.t Incr.t
  ; queue : 'action Action.t Queue.t
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  ; mutable print_actions : bool
  ; stabilization_tracker : 'action Stabilization_tracker.t
  ; running_computation : 'r Bonsai.Private.Computation.t
  ; instrumentation :
      (Instrumentation.Timeable_event.t, 'timer) Bonsai.Private.Instrumentation.Config.t
  ; (* [timer] is split out for convenience. *)
    timer : 'a. Instrumentation.Timeable_event.t -> f:(unit -> 'a) -> 'a
  }

type 'r t = T : (_, _, _, 'r, _) unpacked -> 'r t

let assert_type_equalities
  (T a : _ Bonsai.Private.Computation.packed_info)
  (T b : _ Bonsai.Private.Computation.packed_info)
  =
  let T =
    Bonsai.Private.Meta.Model.Type_id.same_witness_exn a.model.type_id b.model.type_id
  in
  let T = Bonsai.Private.Action.Type_id.same_witness_exn a.action b.action in
  ()
;;

let assert_unoptimized_type_equalities ~time_source computation =
  let unoptimized_info =
    Bonsai.Private.gather
      ~recursive_scopes:Bonsai.Private.Computation.Recursive_scopes.empty
      ~time_source
      computation
  in
  (* This check is intentionally comparing [unoptimized_info] with itself to make sure it
     doesn't raise. *)
  assert_type_equalities unoptimized_info unoptimized_info
;;

let[@inline never] define_ui_effect (type action) () =
  let queue = Queue.create () in
  let module A =
    Ui_effect.Define (struct
      module Action = struct
        type t = action Action.t
      end

      let handle value ~on_exn:_ = Queue.enqueue queue value
    end)
  in
  let inject = A.inject in
  inject, queue
;;

let create_direct
  (type r)
  ~(here : [%call_pos])
  ?(optimize = true)
  ~(instrumentation :
      (Instrumentation.Timeable_event.t, _) Bonsai.Private.Instrumentation.Config.t)
  ~time_source
  (unoptimized_computation : r Bonsai.Private.Computation.t)
  : r t
  =
  let { Timer.time } =
    Timer.create
      ~start_timer:instrumentation.start_timer
      ~stop_timer:instrumentation.stop_timer
  in
  let am_running_bonsai_test =
    am_running_test
    && (String.is_prefix here.pos_fname ~prefix:{|lib/bonsai/test/of_bonsai_itself|}
        || String.is_prefix
             here.pos_fname
             ~prefix:{|lib/bonsai/web_test/of_bonsai_itself|})
  in
  (* This check is mostly here to catch bugs in the implementation of
     [Bonsai.Private.Meta.Model.Type_id.same_witness_exn] and
     [Bonsai.Private.Action.Type_id.same_witness_exn]. We only run it in Bonsai tests to
     avoid slowing down prod, benchmarks, or other users' tests. *)
  if am_running_bonsai_test
  then assert_unoptimized_type_equalities ~time_source unoptimized_computation;
  let running_computation =
    time Preprocess ~f:(fun () ->
      if optimize
      then Bonsai.Private.pre_process unoptimized_computation
      else unoptimized_computation)
  in
  let optimized_info =
    time Gather ~f:(fun () ->
      Bonsai.Private.gather
        running_computation
        ~recursive_scopes:Bonsai.Private.Computation.Recursive_scopes.empty
        ~time_source)
  in
  let (T
        ({ model =
             { default = default_model; sexp_of = sexp_of_model; equal = _; type_id = _ }
         ; input = _
         ; apply_action
         ; action = _
         ; run = _
         ; reset = _
         ; may_contain = _
         } as computation_info))
    =
    optimized_info
  in
  (* This check is intentionally comparing [optimized_info] with itself to make sure it
     doesn't raise. *)
  if am_running_bonsai_test then assert_type_equalities optimized_info optimized_info;
  let environment = Bonsai.Private.Environment.empty in
  let starting_model = default_model in
  let model_var = Incr.Var.create starting_model in
  (* Sadly the only way to give a name to the existential type that we just introduced
     into the environment is by defining a function like this. See
     https://github.com/ocaml/ocaml/issues/7074. *)
  let create_polymorphic
    (type action action_input)
    (computation : r Bonsai.Private.Computation.t)
    (computation_info :
      (_, action, action_input, r, unit) Bonsai.Private.Computation.info)
    apply_action
    : r t
    =
    (* Call [define_ui_effect] twice to prevent inlining *)
    let _ : _ = define_ui_effect () in
    let inject, queue = define_ui_effect () in
    let sexp_of_action = Action.Type_id.to_sexp computation_info.action in
    let result_incr, result, action_input_incr, action_input, lifecycle_incr, lifecycle =
      time Run_eval_fun ~f:(fun () ->
        let%pattern_bind.Ui_incr result_incr, action_input_incr, lifecycle_incr =
          let instrumentation =
            { instrumentation with
              start_timer = (fun s -> instrumentation.start_timer (Profiling_entry s))
            }
          in
          Bonsai.Private.Instrumentation.create_computation_with_instrumentation
            instrumentation
            ~f:(fun run ->
              let snapshot, () =
                run
                  ~environment
                  ~fix_envs:Bonsai.Private.Environment.Recursive.empty
                  ~path:Bonsai.Private.Path.empty
                  ~model:(Incr.Var.watch model_var)
                  ~inject
                |> Bonsai.Private.Trampoline.run
              in
              Ui_incr.map3
                (Bonsai.Private.Snapshot.result snapshot)
                (Bonsai.Private.Input.to_incremental
                   (Bonsai.Private.Snapshot.input snapshot))
                (Bonsai.Private.Snapshot.lifecycle_or_empty ~here:[%here] snapshot)
                ~f:(fun result_incr action_input_incr lifecycle_incr ->
                  result_incr, action_input_incr, lifecycle_incr))
            ~recursive_scopes:Bonsai.Private.Computation.Recursive_scopes.empty
            ~time_source
            ~computation
            computation_info
        in
        let result = Incr.observe result_incr in
        let action_input = Incr.observe action_input_incr in
        let lifecycle = Incr.observe lifecycle_incr in
        result_incr, result, action_input_incr, action_input, lifecycle_incr, lifecycle)
    in
    let stabilization_tracker = Stabilization_tracker.empty () in
    time First_stabilization ~f:(fun () ->
      Incr.stabilize ();
      Stabilization_tracker.mark_stabilization stabilization_tracker);
    T
      { model_var
      ; default_model
      ; time_source
      ; inject
      ; action_input
      ; action_input_incr
      ; apply_action = apply_action ~inject
      ; result
      ; result_incr
      ; sexp_of_model
      ; sexp_of_action
      ; lifecycle
      ; lifecycle_incr
      ; queue
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      ; print_actions = false
      ; stabilization_tracker
      ; running_computation
      ; instrumentation
      ; timer = time
      }
  in
  create_polymorphic running_computation computation_info apply_action
;;

let create
  (type r)
  ~(here : [%call_pos])
  ?(optimize = true)
  ~(instrumentation : _ Bonsai.Private.Instrumentation.Config.t)
  ~time_source
  (computation : local_ Bonsai.graph -> r Bonsai.t)
  =
  let timer =
    Timer.create
      ~start_timer:(fun event ->
        let event =
          match event with
          | `Graph_application -> Instrumentation.Timeable_event.Graph_application
          | `Preprocess -> Preprocess
          | `Gather -> Gather
        in
        instrumentation.start_timer event)
      ~stop_timer:instrumentation.stop_timer
  in
  Timer.set_timer ~timer;
  let graph_applied =
    timer.time `Graph_application ~f:(fun () ->
      Bonsai.Private.top_level_handle computation)
  in
  create_direct ~here ~optimize ~instrumentation ~time_source graph_applied
;;

let schedule_event _ t =
  Ui_effect.Expert.handle t ~on_exn:(fun exn ->
    Exn.reraise exn "Unhandled exception raised in effect")
;;

let has_before_display_events (T t) =
  let lifecycle = t.lifecycle |> Incr.Observer.value_exn in
  Bonsai.Private.Lifecycle.Collection.get_before_display
    ~old:Bonsai.Private.Lifecycle.Collection.empty
    ~new_:lifecycle
  |> Option.is_some
  || Bonsai.Time_source.Private.has_before_display_events t.time_source
;;

let trigger_before_display (T t) ~apply_actions =
  let old = ref Bonsai.Private.Lifecycle.Collection.empty in
  let rec loop () =
    let new_ = t.lifecycle |> Incr.Observer.value_exn in
    match Bonsai.Private.Lifecycle.Collection.get_before_display ~old:!old ~new_ with
    | None -> ()
    | Some before_displays ->
      schedule_event () before_displays;
      old := Map.merge_skewed !old new_ ~combine:(fun ~key:_ l _r -> l);
      apply_actions ();
      loop ()
  in
  loop ();
  if Bonsai.Time_source.Private.has_before_display_events t.time_source
  then (
    Bonsai.Time_source.Private.trigger_before_display t.time_source;
    apply_actions ())
;;

let flush
  ?(log_before_action_application = fun ~action_sexp:_ -> ())
  ?(log_on_skipped_stabilization = fun ~action_sexp:_ -> ())
  (T
    ({ model_var
     ; apply_action
     ; action_input
     ; queue
     ; time_source
     ; stabilization_tracker
     ; sexp_of_action
     ; timer
     ; _
     } as t))
  =
  Bonsai.Time_source.Private.flush time_source;
  timer Stabilize_for_clock ~f:(fun () ->
    Incr.stabilize ();
    Stabilization_tracker.mark_stabilization stabilization_tracker);
  let process_event action model =
    log_before_action_application ~action_sexp:(lazy (sexp_of_action action));
    (* It's important that we don't call [Incr.Var.set] within [process_event] unless
       we're also going to stabilize. Some code in Bonsai relies on this assumption as
       part of its [action_requires_stabilization] logic. Breaking this invariant won't
       break Bonsai code, but it will effectively remove an optimization. *)
    if Stabilization_tracker.requires_stabilization stabilization_tracker action
    then (
      Incr.Var.set model_var model;
      timer Stabilize_for_action ~f:(fun () ->
        Incr.stabilize ();
        Stabilization_tracker.mark_stabilization stabilization_tracker))
    else log_on_skipped_stabilization ~action_sexp:(lazy (sexp_of_action action));
    let new_model =
      let action_input = Incr.Observer.value_exn action_input in
      apply_action
        ~schedule_event:
          (Ui_effect.Expert.handle ~on_exn:(fun exn ->
             Exn.reraise exn "Unhandled exception raised in effect"))
        (Some action_input)
        model
        action
    in
    Stabilization_tracker.insert stabilization_tracker action;
    if t.print_actions then print_s [%message "Processed action" (action : action)];
    new_model
  in
  let rec apply_actions model =
    match Queue.dequeue queue with
    | None ->
      Incr.Var.set model_var model;
      timer Stabilize_after_all_apply_actions ~f:(fun () ->
        Incr.stabilize ();
        Stabilization_tracker.mark_stabilization stabilization_tracker)
    | Some action ->
      let new_model = process_event action model in
      apply_actions new_model
  in
  (* The only difference between [Var.latest_value] and [Var.value] is that if [Var.set]
     is called _while stabilizing_, then calling [Var.value] will return the value that
     was set when stabilization started, whereas [latest_value] will give you the value
     that was just [set]. Now, setting a model in the middle of a stabilization should
     never happen, but I think it's important to be explicit about which behavior we use,
     so I chose the one that would be least surprising if a stabilization does happen to
     occur. *)
  let apply_actions () =
    timer Apply_actions ~f:(fun () -> apply_actions (Incr.Var.latest_value model_var))
  in
  apply_actions ();
  trigger_before_display (T t) ~apply_actions
;;

let result (T { result; _ }) = Incr.Observer.value_exn result

let has_after_display_events (T t) =
  let lifecycle = t.lifecycle |> Incr.Observer.value_exn in
  Bonsai.Private.Lifecycle.Collection.has_after_display lifecycle
  || Bonsai.Time_source.Private.has_after_display_events t.time_source
;;

let trigger_lifecycles (T t) =
  let old = t.last_lifecycle in
  let new_ = t.lifecycle |> Incr.Observer.value_exn in
  t.last_lifecycle <- new_;
  schedule_event () (Bonsai.Private.Lifecycle.Collection.get_after_display ~old ~new_);
  Bonsai.Time_source.Private.trigger_after_display t.time_source
;;

module Expert = struct
  let sexp_of_model (T { sexp_of_model; model_var; _ }) =
    sexp_of_model (Incr.Var.value model_var)
  ;;

  let result_incr (T { result_incr; _ }) = result_incr
  let action_input_incr (T { action_input_incr; _ }) = Ui_incr.pack action_input_incr
  let lifecycle_incr (T { lifecycle_incr; _ }) = Ui_incr.pack lifecycle_incr
  let time_source (T { time_source; _ }) = time_source

  let invalidate_observers (T { action_input; result; lifecycle; _ }) =
    Incr.Observer.disallow_future_use action_input;
    Incr.Observer.disallow_future_use result;
    Incr.Observer.disallow_future_use lifecycle
  ;;

  let reset_model_to_default (T { default_model; model_var; _ }) =
    Incr.Var.set model_var default_model
  ;;

  let print_actions (T t) = t.print_actions <- true

  let print_stabilizations (T t) =
    Stabilization_tracker.For_testing.start_debugging t.stabilization_tracker
  ;;

  let print_stabilization_tracker_stats (T t) =
    Stabilization_tracker.For_testing.display_stats t.stabilization_tracker
  ;;
end

module For_testing = struct
  let dump_dot (T { result_incr; lifecycle_incr; action_input_incr; _ }) =
    let buffer = Buffer.create 10 in
    let formatter = Format.formatter_of_buffer buffer in
    Ui_incr.Packed.save_dot
      formatter
      [ Incr.pack result_incr; Incr.pack lifecycle_incr; Incr.pack action_input_incr ];
    Buffer.contents buffer
  ;;

  let running_computation (T { running_computation; _ }) = running_computation
end
