open! Core
open! Import

module Profiled_event = struct
  module Node_id = String

  module Metadata = struct
    type t =
      | Map of { arity : int }
      | Assoc_by
      | Incr_compute
      | Apply_action of
          { action_sexp : Sexp.t option
          ; kind : [ `Static | `Dynamic | `Wrap ]
          }
    [@@deriving sexp, equal]

    let node_type = function
      | Map { arity = 1 } -> "map"
      | Map { arity = n } -> [%string "map%{n#Int}"]
      | Assoc_by -> "assoc_simpl"
      | Incr_compute -> "leaf_incr"
      | Apply_action { kind; _ } ->
        (match kind with
         | `Static -> "leaf0"
         | `Dynamic -> "leaf1"
         | `Wrap -> "wrap")
    ;;
  end

  type t =
    { node_id : Node_id.t
    ; metadata : Metadata.t
    ; here : Source_code_position.Stable.V1.t
    }
  [@@deriving sexp, equal]

  let to_entry_label t = "##" ^ Sexp.to_string_mach [%sexp (t : t)]

  let of_entry_label s =
    let%bind.Option s = String.chop_prefix s ~prefix:"##" in
    Option.try_with (fun () -> s |> Sexp.of_string |> t_of_sexp)
  ;;

  module For_testing = struct
    let to_string { node_id; metadata; here } =
      [%string
        "##%{Metadata.node_type metadata} %{here#Source_code_position} %{node_id#String}"]
    ;;
  end
end

let instrument_for_measuring_timings (t : _ Computation.t) ~start_timer ~stop_timer =
  let { Timer.time } = Timer.create ~start_timer ~stop_timer in
  let computation_map
    (type result)
    (context : unit Transform.For_computation.context)
    ()
    (computation : result Computation.t)
    : result Computation.t Trampoline.t
    =
    let node_id =
      let%map.Lazy node_path = context.current_path in
      Node_path.to_string node_path
    in
    let here = Computation.source_code_position computation in
    let open Trampoline.Let_syntax in
    let%bind recursed = context.recurse () computation in
    match recursed with
    | Fetch v_id -> return (Computation.Fetch v_id)
    | Leaf0 t ->
      let apply_action ~inject ~schedule_event ~time_source model action =
        let action_sexp =
          let%map.Option sexp_of_action = t.sexp_of_action in
          sexp_of_action action
        in
        let event : Profiled_event.t =
          { node_id = force node_id
          ; metadata = Apply_action { action_sexp; kind = `Static }
          ; here
          }
        in
        time event ~f:(fun () ->
          t.apply_action ~inject ~schedule_event ~time_source model action)
      in
      return (Computation.Leaf0 { t with apply_action })
    | Leaf1 t ->
      let apply_action ~inject ~schedule_event ~time_source input model action =
        let action_sexp =
          let%map.Option sexp_of_action = t.sexp_of_action in
          sexp_of_action action
        in
        let event : Profiled_event.t =
          { node_id = force node_id
          ; metadata = Apply_action { action_sexp; kind = `Dynamic }
          ; here
          }
        in
        time event ~f:(fun () ->
          t.apply_action ~inject ~schedule_event ~time_source input model action)
      in
      return (Computation.Leaf1 { t with apply_action })
    | Wrap t ->
      let dynamic_apply_action ~inject ~schedule_event ~time_source input model action =
        let event : Profiled_event.t =
          { node_id = force node_id
          ; metadata = Apply_action { action_sexp = None; kind = `Wrap }
          ; here
          }
        in
        time event ~f:(fun () ->
          t.dynamic_apply_action ~inject ~schedule_event ~time_source input model action)
      in
      return (Computation.Wrap { t with dynamic_apply_action })
    | Leaf_incr t ->
      let event : Profiled_event.t =
        { node_id = force node_id; metadata = Incr_compute; here }
      in
      let compute clock input = time event ~f:(fun () -> t.compute clock input) in
      return (Computation.Leaf_incr { t with compute })
    | Assoc_simpl t ->
      let event : Profiled_event.t =
        { node_id = force node_id; metadata = Assoc_by; here }
      in
      let by path key value = time event ~f:(fun () -> t.by path key value) in
      return (Computation.Assoc_simpl { t with by })
    | computation -> return computation
  in
  let value_map
    (type a)
    (context : unit Transform.For_value.context)
    ()
    ({ here; value } : a Value.t)
    =
    let node_id =
      let%map.Lazy node_path = context.current_path in
      Node_path.to_string node_path
    in
    let event arity : Profiled_event.t =
      { node_id = force node_id; metadata = Map { arity }; here }
    in
    let value =
      match value with
      | Constant _ | Exception _ | Incr _ | Named _ | Both (_, _) | Cutoff _ -> value
      | Map t ->
        let f a = time (event 1) ~f:(fun () -> t.f a) in
        Map { t with f }
      | Map2 t ->
        let f a b = time (event 2) ~f:(fun () -> t.f a b) in
        Map2 { t with f }
      | Map3 t ->
        let f a b c = time (event 3) ~f:(fun () -> t.f a b c) in
        Map3 { t with f }
      | Map4 t ->
        let f a b c d = time (event 4) ~f:(fun () -> t.f a b c d) in
        Map4 { t with f }
      | Map5 t ->
        let f a b c d e = time (event 5) ~f:(fun () -> t.f a b c d e) in
        Map5 { t with f }
      | Map6 t ->
        let f a b c d e f = time (event 6) ~f:(fun () -> t.f a b c d e f) in
        Map6 { t with f }
      | Map7 t ->
        let f a b c d e f g = time (event 7) ~f:(fun () -> t.f a b c d e f g) in
        Map7 { t with f }
    in
    context.recurse () { here; value }
  in
  Transform.map
    ~init:()
    ~computation_mapper:{ f = computation_map }
    ~value_mapper:{ f = value_map }
    t
;;

module Profiling = struct
  type t =
    | Profiling
    | Not_profiling
  [@@deriving sexp]
end

module Watching = struct
  type t =
    | Watching
    | Not_watching
  [@@deriving sexp_of]
end

module Config = struct
  type ('timeable_event, 'timer) t =
    { instrument_for_computation_watcher : Watching.t Incr.t
    ; instrument_for_profiling : Profiling.t Incr.t
    ; computation_watcher_queue : Computation_watcher.Node.t Queue.t
    ; start_timer : 'timeable_event -> 'timer
    ; stop_timer : 'timer -> unit
    }
end

let instrument_for_profiling ~start_timer ~stop_timer computation =
  instrument_for_measuring_timings computation ~start_timer ~stop_timer
;;

let create_computation_with_instrumentation
  (type action_input model action result a)
  { Config.instrument_for_profiling = enable_instrument_for_profiling
  ; instrument_for_computation_watcher
  ; computation_watcher_queue
  ; start_timer
  ; stop_timer
  }
  ~(f : (model, action, action_input, result, unit) Computation.info -> a Ui_incr.Incr.t)
  ~(recursive_scopes : Computation.Recursive_scopes.t)
  ~(time_source : Time_source.t)
  ~(computation : result Computation.t)
  (info : (model, action, action_input, result, unit) Computation.info)
  : a Ui_incr.Incr.t
  =
  let open Ui_incr.Incr.Let_syntax in
  let gather_and_assert_typechecks ~what instrumented_computation =
    let (T info') =
      instrumented_computation |> Gather.gather ~recursive_scopes ~time_source
    in
    match
      Meta.(
        ( Model.Type_id.same_witness info.model.type_id info'.model.type_id
        , Action.Type_id.same_witness info.action info'.action
        , Input.same_witness info.input info'.input ))
    with
    | Some T, Some T, Some T ->
      (info' : (model, action, action_input, result, unit) Computation.info)
    | _ ->
      print_endline
        [%string
          "Not starting %{what}. An error occurred while attempting to instrument the \
           computation; the resulting computation does not typecheck. Reusing previously \
           gathered run information to execute"];
      info
  in
  let instrument_for_profiling = instrument_for_profiling ~start_timer ~stop_timer in
  (* We make these lazy so that, after turning recording on once, the recording button can
     be quickly toggled/we don't have to do this computation multiple times. *)
  let run_profiling_and_watching =
    lazy
      (instrument_for_profiling computation
       |> Enable_computation_watcher.run ~watcher_queue:computation_watcher_queue
       |> gather_and_assert_typechecks ~what:"Bonsai profiler or computation watcher")
  in
  let run_profiling_not_watching =
    lazy
      (instrument_for_profiling computation
       |> gather_and_assert_typechecks ~what:"computation watcher")
  in
  let run_watching_not_profiling =
    lazy
      (Enable_computation_watcher.run computation ~watcher_queue:computation_watcher_queue
       |> gather_and_assert_typechecks ~what:"Bonsai profiler")
  in
  match%bind
    Ui_incr.both enable_instrument_for_profiling instrument_for_computation_watcher
  with
  | Profiling, Watching -> f (Lazy.force run_profiling_and_watching)
  | Profiling, Not_watching -> f (Lazy.force run_profiling_not_watching)
  | Not_profiling, Watching -> f (Lazy.force run_watching_not_profiling)
  | Not_profiling, Not_watching -> f info
;;

module For_testing = struct
  let instrument_for_profiling = instrument_for_profiling
end
