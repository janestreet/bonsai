open! Core
open! Import

module Entry_label = struct
  (* The label that gets attached to performance measurements. It shows up in
     the flamegraph produced by the chrome profiler, and also in the messages
     that we receive from the [PerformanceObserver]. Since this label may be
     viewed by humans, we include the [node_type] field and hand-craft the sexp
     representation. *)

  let to_string ~id ~node_type = [%string "##%{node_type} %{id}"]
end

let extract_node_path_from_entry_label label =
  if String.is_prefix ~prefix:"##" label
  then (
    match String.split label ~on:' ' with
    | [ _; node_path ] -> Some (Node_path.of_string node_path)
    | _ -> None)
  else None
;;

let instrument_computation (t : _ Computation.t) ~start_timer ~stop_timer =
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
    let entry_label node_type = Entry_label.to_string ~id ~node_type in
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
      let handle = start_timer apply_action_label in
      let model = apply_action ~inject ~schedule_event ~time_source input model action in
      stop_timer handle;
      model
    in
    let time_static_apply_action
      ~apply_action
      ~inject
      ~schedule_event
      ~time_source
      model
      action
      =
      let handle = start_timer apply_action_label in
      let model = apply_action ~inject ~schedule_event ~time_source model action in
      stop_timer handle;
      model
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
      let compute clock input =
        let handle = start_timer compute_label in
        let computed = t.compute clock input in
        stop_timer handle;
        computed
      in
      return (Computation.Leaf_incr { t with compute })
    | Assoc_simpl t ->
      let by path key value =
        let handle = start_timer by_label in
        let by = t.by path key value in
        stop_timer handle;
        by
      in
      return (Computation.Assoc_simpl { t with by })
    | computation -> return computation
  in
  let value_map
    (type a)
    (context : unit Transform.For_value.context)
    ()
    ({ here; value; id } as wrapped_value : a Value.t)
    =
    let (lazy current_path) = context.current_path in
    let node_info = Graph_info.Node_info.of_value wrapped_value in
    let entry_label =
      let id = Node_path.to_string current_path in
      Entry_label.to_string ~id ~node_type:node_info.node_type
    in
    let value =
      match value with
      | Constant _ | Exception _ | Incr _ | Named _ | Both (_, _) | Cutoff _ -> value
      | Map t ->
        let f a =
          let handle = start_timer entry_label in
          let x = t.f a in
          stop_timer handle;
          x
        in
        Map { t with f }
      | Map2 t ->
        let f a b =
          let handle = start_timer entry_label in
          let x = t.f a b in
          stop_timer handle;
          x
        in
        Map2 { t with f }
      | Map3 t ->
        let f a b c =
          let handle = start_timer entry_label in
          let x = t.f a b c in
          stop_timer handle;
          x
        in
        Map3 { t with f }
      | Map4 t ->
        let f a b c d =
          let handle = start_timer entry_label in
          let x = t.f a b c d in
          stop_timer handle;
          x
        in
        Map4 { t with f }
      | Map5 t ->
        let f a b c d e =
          let handle = start_timer entry_label in
          let x = t.f a b c d e in
          stop_timer handle;
          x
        in
        Map5 { t with f }
      | Map6 t ->
        let f a b c d e f =
          let handle = start_timer entry_label in
          let x = t.f a b c d e f in
          stop_timer handle;
          x
        in
        Map6 { t with f }
      | Map7 t ->
        let f a b c d e f g =
          let handle = start_timer entry_label in
          let x = t.f a b c d e f g in
          stop_timer handle;
          x
        in
        Map7 { t with f }
    in
    context.recurse () { here; value; id }
  in
  Transform.map
    ~init:()
    ~computation_mapper:{ f = computation_map }
    ~value_mapper:{ f = value_map }
    t
;;
