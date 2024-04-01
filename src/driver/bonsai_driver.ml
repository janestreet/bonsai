open! Core
module Incr = Ui_incr
module Stabilization_tracker = Bonsai.Private.Stabilization_tracker
module Action = Bonsai.Private.Action

type ('m, 'action, 'action_input, 'r) unpacked =
  { model_var : 'm Incr.Var.t
  ; default_model : 'm
  ; clock : Bonsai.Time_source.t
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
  }

type 'r t = T : (_, _, _, 'r) unpacked -> 'r t

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

let create_direct
  (type r)
  ?(optimize = true)
  ~clock
  (computation : r Bonsai.Private.Computation.t)
  : r t
  =
  let unoptimized_info = Bonsai.Private.gather computation in
  let optimized_info =
    computation
    |> (if optimize then Bonsai.Private.pre_process else Fn.id)
    |> Bonsai.Private.gather
  in
  let (T
        ({ model =
             { default = default_model; sexp_of = sexp_of_model; equal = _; type_id = _ }
         ; input = _
         ; apply_action
         ; action = _
         ; run = _
         ; reset = _
         ; may_contain_lifecycle = _
         ; may_contain_path = _
         } as computation_info))
    =
    optimized_info
  in
  assert_type_equalities unoptimized_info unoptimized_info;
  assert_type_equalities optimized_info optimized_info;
  let environment = Bonsai.Private.Environment.empty in
  let starting_model = default_model in
  let model_var = Incr.Var.create starting_model in
  (* Sadly the only way to give a name to the existential type that we just introduced
     into the environment is by defining a function like this. See
     https://github.com/ocaml/ocaml/issues/7074. *)
  let create_polymorphic
    (type action action_input)
    (computation_info :
      (_, action, action_input, r, unit) Bonsai.Private.Computation.info)
    apply_action
    : r t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = action Action.t
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let sexp_of_action = Action.Type_id.to_sexp computation_info.action in
    let snapshot, () =
      computation_info.run
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject
      |> Bonsai.Private.Trampoline.run
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let action_input_incr =
      Bonsai.Private.Input.to_incremental (Bonsai.Private.Snapshot.input snapshot)
    in
    let action_input = Incr.observe action_input_incr in
    let result = result_incr |> Incr.observe in
    let lifecycle_incr = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
    let lifecycle = Incr.observe lifecycle_incr in
    Incr.stabilize ();
    T
      { model_var
      ; default_model
      ; clock
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
      ; stabilization_tracker = Stabilization_tracker.empty ()
      }
  in
  create_polymorphic computation_info apply_action
;;

let create (type r) ?(optimize = true) ~clock (computation : r Bonsai.Computation.t) =
  create_direct ~optimize ~clock (Bonsai.Private.top_level_handle computation)
;;

let schedule_event _ = Ui_effect.Expert.handle

let flush
  (T
    ({ model_var
     ; apply_action
     ; action_input
     ; queue
     ; clock
     ; stabilization_tracker
     ; sexp_of_action
     ; _
     } as t))
  =
  Bonsai.Time_source.Private.flush clock;
  let process_event action model =
    if Stabilization_tracker.requires_stabilization stabilization_tracker action
    then (
      Incr.Var.set model_var model;
      Incr.stabilize ();
      Stabilization_tracker.mark_stabilization stabilization_tracker);
    let new_model =
      let action_input = Incr.Observer.value_exn action_input in
      apply_action
        ~schedule_event:Ui_effect.Expert.handle
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
      Incr.stabilize ();
      Stabilization_tracker.mark_stabilization stabilization_tracker
    | Some action ->
      let new_model = process_event action model in
      apply_actions new_model
  in
  (* The only difference between [Var.latest_value] and [Var.value] is that
     if [Var.set] is called _while stabilizing_, then calling [Var.value]
     will return the value that was set when stabilization started, whereas
     [latest_value] will give you the value that was just [set].  Now,
     setting a model in the middle of a stabilization should never happen,
     but I think it's important to be explicit about which behavior we use,
     so I chose the one that would be least surprising if a stabilization
     does happen to occur. *)
  apply_actions (Incr.Var.latest_value model_var)
;;

let result (T { result; _ }) = Incr.Observer.value_exn result

let has_after_display_events (T t) =
  let lifecycle = t.lifecycle |> Incr.Observer.value_exn in
  Bonsai.Private.Lifecycle.Collection.has_after_display lifecycle
  || Bonsai.Time_source.Private.has_after_display_events t.clock
;;

let trigger_lifecycles (T t) =
  let old = t.last_lifecycle in
  let new_ = t.lifecycle |> Incr.Observer.value_exn in
  t.last_lifecycle <- new_;
  schedule_event () (Bonsai.Private.Lifecycle.Collection.diff old new_);
  Bonsai.Time_source.Private.trigger_after_display t.clock
;;

module Expert = struct
  let sexp_of_model (T { sexp_of_model; model_var; _ }) =
    sexp_of_model (Incr.Var.value model_var)
  ;;

  let result_incr (T { result_incr; _ }) = result_incr
  let action_input_incr (T { action_input_incr; _ }) = Ui_incr.pack action_input_incr
  let lifecycle_incr (T { lifecycle_incr; _ }) = Ui_incr.pack lifecycle_incr
  let clock (T { clock; _ }) = clock

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
    let tempfile = "/tmp/dump.dot" in
    Out_channel.with_file tempfile ~f:(fun oc ->
      Ui_incr.Packed.save_dot
        oc
        [ Incr.pack result_incr; Incr.pack lifecycle_incr; Incr.pack action_input_incr ]);
    In_channel.read_all tempfile
  ;;
end
