open! Core
open! Import

module Entry_label = struct
  (* The label that gets attached to performance measurements. It shows up in
     the flamegraph produced by the chrome profiler, and also in the messages
     that we receive from the [PerformanceObserver]. Since this label may be
     viewed by humans, we include the [node_type] field and hand-craft the sexp
     representation. *)
  type t =
    { id : Node_path.t
    ; node_type : string
    }

  let sexp_of_t { id; node_type } = [%sexp (id : Node_path.t), (node_type : string)]
  let to_string t = Sexp.to_string (sexp_of_t t)
end

let instrument_computation (t : (_, _, _) Computation.t) ~start_timer ~stop_timer =
  let computation_map
        (type b c d)
        ~(recurse : unit Transform.For_computation.mapper)
        ~var_from_parent:_
        ~parent_path:_
        ~current_path
        ()
        (computation : (b, c, d) Computation.t)
    : (b, c, d) Computation.t
    =
    let node_info = Graph_info.Node_info.of_computation computation in
    let entry_label node_type =
      { Entry_label.id = current_path; node_type } |> Entry_label.to_string
    in
    let compute_label = entry_label [%string "%{node_info.node_type}-compute"] in
    let apply_action_label =
      entry_label [%string "%{node_info.node_type}-apply_action"]
    in
    let by_label = entry_label [%string "%{node_info.node_type}-by"] in
    let time_apply_action ~apply_action ~inject ~schedule_event input model action =
      start_timer apply_action_label;
      let model = apply_action ~inject ~schedule_event input model action in
      stop_timer apply_action_label;
      model
    in
    match recurse.f () computation with
    | Fetch v_id -> Fetch v_id
    | Leaf1 { input; apply_action; compute; name; kind } ->
      let compute ~inject input model =
        start_timer compute_label;
        let computed = compute ~inject input model in
        stop_timer compute_label;
        computed
      in
      Leaf1 { input; apply_action = time_apply_action ~apply_action; compute; name; kind }
    | Leaf0 { apply_action; compute; name; kind } ->
      let compute ~inject model =
        start_timer compute_label;
        let computed = compute ~inject model in
        stop_timer compute_label;
        computed
      in
      Leaf0 { apply_action = time_apply_action ~apply_action; name; kind; compute }
    | Leaf_incr { input; apply_action; compute; name } ->
      let apply_action input ~inject =
        start_timer apply_action_label;
        let model_incr = apply_action input ~inject in
        stop_timer apply_action_label;
        model_incr
      in
      let compute clock input model ~inject =
        start_timer compute_label;
        let computed = compute clock input model ~inject in
        stop_timer compute_label;
        computed
      in
      Leaf_incr { input; apply_action; compute; name }
    | Assoc_simpl t ->
      let by path key value =
        start_timer by_label;
        let by = t.by path key value in
        stop_timer by_label;
        by
      in
      Assoc_simpl { t with by }
    | c -> c
  in
  let value_map
        (type a)
        ~(recurse : unit Transform.For_value.mapper)
        ~var_from_parent:_
        ~parent_path:_
        ~current_path
        ()
        ({ here; value } as wrapped_value : a Value.t)
    =
    let node_info = Graph_info.Node_info.of_value wrapped_value in
    let entry_label =
      { Entry_label.id = current_path; node_type = node_info.node_type }
      |> Entry_label.to_string
    in
    let value =
      match value with
      | Constant (_, _) | Incr _ | Named _ | Both (_, _) | Cutoff _ -> value
      | Map t ->
        let f a =
          start_timer entry_label;
          let x = t.f a in
          stop_timer entry_label;
          x
        in
        Map { t with f }
      | Map2 t ->
        let f a b =
          start_timer entry_label;
          let x = t.f a b in
          stop_timer entry_label;
          x
        in
        Map2 { t with f }
      | Map3 t ->
        let f a b c =
          start_timer entry_label;
          let x = t.f a b c in
          stop_timer entry_label;
          x
        in
        Map3 { t with f }
      | Map4 t ->
        let f a b c d =
          start_timer entry_label;
          let x = t.f a b c d in
          stop_timer entry_label;
          x
        in
        Map4 { t with f }
      | Map5 t ->
        let f a b c d e =
          start_timer entry_label;
          let x = t.f a b c d e in
          stop_timer entry_label;
          x
        in
        Map5 { t with f }
      | Map6 t ->
        let f a b c d e f =
          start_timer entry_label;
          let x = t.f a b c d e f in
          stop_timer entry_label;
          x
        in
        Map6 { t with f }
      | Map7 t ->
        let f a b c d e f g =
          start_timer entry_label;
          let x = t.f a b c d e f g in
          stop_timer entry_label;
          x
        in
        Map7 { t with f }
    in
    recurse.f () { here; value }
  in
  Transform.map
    ~init:()
    ~computation_mapper:{ f = computation_map }
    ~value_mapper:{ f = value_map }
    t
;;

let instrument_packed (Computation.T { t; action; model }) ~start_timer ~stop_timer =
  Computation.T { t = instrument_computation ~start_timer ~stop_timer t; action; model }
;;
