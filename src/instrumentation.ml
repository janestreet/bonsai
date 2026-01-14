open! Core
open! Import

module Entry_label = struct
  (* The label that gets attached to performance measurements. It shows up in the
     flamegraph produced by the chrome profiler, and also in the messages that we receive
     from the [PerformanceObserver]. Since this label may be viewed by humans, we include
     the [node_type] field and hand-craft the sexp representation. *)

  let to_string ~here ~id ~node_type =
    [%string "##%{node_type} %{here#Source_code_position} %{id}"]
  ;;
end

let extract_node_path_from_entry_label label =
  if String.is_prefix ~prefix:"##" label
  then (
    match String.split label ~on:' ' with
    | [ _; node_path ] | _ :: _ :: node_path :: _ -> Some (Node_path.of_string node_path)
    | _ -> None)
  else None
;;

let instrument_for_measuring_timings (t : _ Computation.t) ~start_timer ~stop_timer =
  let { Timer.time } = Timer.create ~start_timer ~stop_timer in
  let computation_map
    (type result)
    (context : unit Transform.For_computation.context)
    ()
    (computation : result Computation.t)
    : result Computation.t Trampoline.t
    =
    let node_info = Graph_info.Node_info.of_computation computation in
    let id =
      let (lazy current_path) = context.current_path in
      Node_path.to_string current_path
    in
    let entry_label node_type =
      Entry_label.to_string
        ~here:(Computation.source_code_position computation)
        ~id
        ~node_type
    in
    let compute_label = entry_label [%string "%{node_info.node_type}-compute"] in
    let apply_action_label =
      entry_label [%string "%{node_info.node_type}-apply_action"]
    in
    let by_label = entry_label [%string "%{node_info.node_type}-by"] in
    let time_apply_action
      ~apply_action
      ~inject
      ~schedule_event
      ~time_source
      input
      model
      action
      =
      time apply_action_label ~f:(fun () ->
        apply_action ~inject ~schedule_event ~time_source input model action)
    in
    let time_static_apply_action
      ~apply_action
      ~inject
      ~schedule_event
      ~time_source
      model
      action
      =
      time apply_action_label ~f:(fun () ->
        apply_action ~inject ~schedule_event ~time_source model action)
    in
    let open Trampoline.Let_syntax in
    let%bind recursed = context.recurse () computation in
    match recursed with
    | Fetch v_id -> return (Computation.Fetch v_id)
    | Leaf1 t ->
      return
        (Computation.Leaf1
           { t with apply_action = time_apply_action ~apply_action:t.apply_action })
    | Leaf0 t ->
      return
        (Computation.Leaf0
           { t with apply_action = time_static_apply_action ~apply_action:t.apply_action })
    | Leaf_incr t ->
      let compute clock input = time compute_label ~f:(fun () -> t.compute clock input) in
      return (Computation.Leaf_incr { t with compute })
    | Assoc_simpl t ->
      let by path key value = time by_label ~f:(fun () -> t.by path key value) in
      return (Computation.Assoc_simpl { t with by })
    | computation -> return computation
  in
  let value_map
    (type a)
    (context : unit Transform.For_value.context)
    ()
    ({ here; value } as value' : a Value.t)
    =
    let entry_label =
      lazy
        (let id = Node_path.to_string (force context.current_path) in
         let node_info = Graph_info.Node_info.of_value value' in
         Entry_label.to_string ~here ~id ~node_type:node_info.node_type)
    in
    let value =
      match value with
      | Constant _ | Exception _ | Incr _ | Named _ | Both (_, _) | Cutoff _ -> value
      | Map t ->
        let f a = time (force entry_label) ~f:(fun () -> t.f a) in
        Map { t with f }
      | Map2 t ->
        let f a b = time (force entry_label) ~f:(fun () -> t.f a b) in
        Map2 { t with f }
      | Map3 t ->
        let f a b c = time (force entry_label) ~f:(fun () -> t.f a b c) in
        Map3 { t with f }
      | Map4 t ->
        let f a b c d = time (force entry_label) ~f:(fun () -> t.f a b c d) in
        Map4 { t with f }
      | Map5 t ->
        let f a b c d e = time (force entry_label) ~f:(fun () -> t.f a b c d e) in
        Map5 { t with f }
      | Map6 t ->
        let f a b c d e f = time (force entry_label) ~f:(fun () -> t.f a b c d e f) in
        Map6 { t with f }
      | Map7 t ->
        let f a b c d e f g = time (force entry_label) ~f:(fun () -> t.f a b c d e f g) in
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
    ; set_latest_graph_info : Graph_info.Stable.V3.t -> unit
    ; computation_watcher_queue : Computation_watcher.Node.t Queue.t
    ; start_timer : 'timeable_event -> 'timer
    ; stop_timer : 'timer -> unit
    }
end

let instrument_for_profiling ~set_latest_graph_info ~start_timer ~stop_timer computation =
  Graph_info.iter_graph_updates computation ~on_update:set_latest_graph_info
  |> instrument_for_measuring_timings ~start_timer ~stop_timer
;;

let create_computation_with_instrumentation
  (type action_input model action result a)
  { Config.instrument_for_profiling = enable_instrument_for_profiling
  ; instrument_for_computation_watcher
  ; set_latest_graph_info
  ; computation_watcher_queue
  ; start_timer
  ; stop_timer
  }
  ~(f :
      (model, action, action_input, result, unit) Computation.eval_fun -> a Ui_incr.Incr.t)
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
      (info' : (model, action, action_input, result, unit) Computation.info).run
    | _ ->
      print_endline
        [%string
          "Not starting %{what}. An error occurred while attempting to instrument the \
           computation; the resulting computation does not typecheck. Reusing previously \
           gathered run information to execute"];
      info.run
  in
  let instrument_for_profiling =
    instrument_for_profiling ~set_latest_graph_info ~start_timer ~stop_timer
  in
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
  | Not_profiling, Not_watching -> f info.run
;;

module For_testing = struct
  let instrument_for_profiling = instrument_for_profiling
end
