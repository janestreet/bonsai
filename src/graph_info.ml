open! Core
open! Import

module Source_code_position = struct
  (* We copy [Source_code_position] so we can derive [sexp] and [bin_io] on it. *)
  type t = Source_code_position.t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp, bin_io]
end

module Node_info = struct
  type t =
    { node_type : string
    ; here : Source_code_position.t option
    }
  [@@deriving sexp, bin_io]

  let of_value (type a) ({ value; here } : a Value.t) =
    let node_type =
      match value with
      | Constant (_, _) -> "constant"
      | Incr _ -> "incr"
      | Named _ -> "named"
      | Both (_, _) -> "both"
      | Cutoff _ -> "cutoff"
      | Map _ -> "map"
      | Map2 _ -> "map2"
      | Map3 _ -> "map3"
      | Map4 _ -> "map4"
      | Map5 _ -> "map5"
      | Map6 _ -> "map6"
      | Map7 _ -> "map7"
    in
    { node_type; here }
  ;;

  let of_computation
        (type model dynamic_action static_action result)
        (computation : (model, dynamic_action, static_action, result) Computation.t)
    =
    let here =
      match computation with
      | Subst { here; _ }
      | Subst_stateless_from { here; _ }
      | Subst_stateless_into { here; _ } -> here
      | _ -> None
    in
    let node_type =
      match computation with
      | Return _ -> "return"
      | Leaf1 _ -> "leaf1"
      | Leaf0 _ -> "leaf0"
      | Leaf_incr _ -> "leaf_incr"
      | Model_cutoff _ -> "model_cutoff"
      | Subst _ -> "subst"
      | Subst_stateless_from _ -> "subst_stateless_from"
      | Subst_stateless_into _ -> "subst_stateless_into"
      | Store _ -> "store"
      | Fetch _ -> "fetch"
      | Assoc _ -> "assoc"
      | Assoc_simpl _ -> "assoc_simpl"
      | Switch _ -> "switch"
      | Lazy _ -> "lazy"
      | Wrap _ -> "wrap"
      | With_model_resetter _ -> "with_model_resetter"
      | Path -> "path"
      | Lifecycle _ -> "lifecycle"
    in
    { node_type; here }
  ;;
end

type t =
  { tree : Node_path.t Node_path.Map.t
  ; dag : Node_path.t list Node_path.Map.t
  ; info : Node_info.t Node_path.Map.t
  }
[@@deriving bin_io, sexp]

let empty =
  { tree = Node_path.Map.empty; dag = Node_path.Map.empty; info = Node_path.Map.empty }
;;

let value_map
      (type a)
      ~(recurse : _ Transform.For_value.mapper)
      ~var_from_parent
      ~parent_path
      ~current_path
      state
      ({ here; value } as wrapped_value : a Value.t)
  =
  let environment, add_tree_relationship, add_dag_relationship = state in
  let node_info = Node_info.of_value wrapped_value in
  (match var_from_parent with
   | Transform.Var_from_parent.One var_from_parent ->
     Hashtbl.set environment ~key:var_from_parent ~data:current_path
   | Two (fst, snd) ->
     Hashtbl.set environment ~key:fst ~data:current_path;
     Hashtbl.set environment ~key:snd ~data:current_path
   | None -> ());
  add_tree_relationship ~from:current_path ~to_:parent_path ~from_info:node_info;
  add_dag_relationship ~from:current_path ~to_:parent_path;
  let value =
    match value with
    | Named id ->
      (match Hashtbl.find environment (Type_equal.Id.uid id) with
       | Some named_id -> add_dag_relationship ~from:named_id ~to_:current_path
       | None -> print_s [%message "BUG" [%here]]);
      value
    | v -> v
  in
  recurse.f state { here; value }
;;

let computation_map
      (type model dynamic_action static_action result)
      ~(recurse : _ Transform.For_computation.mapper)
      ~var_from_parent
      ~parent_path
      ~current_path
      state
      (computation : (model, dynamic_action, static_action, result) Computation.t)
  : (model, dynamic_action, static_action, result) Computation.t
  =
  let environment, add_tree_relationship, add_dag_relationship = state in
  let node_info = Node_info.of_computation computation in
  add_tree_relationship ~from:current_path ~to_:parent_path ~from_info:node_info;
  add_dag_relationship ~from:current_path ~to_:parent_path;
  (match var_from_parent with
   | Transform.Var_from_parent.One var_from_parent ->
     Hashtbl.set environment ~key:var_from_parent ~data:current_path
   | Two (fst, snd) ->
     Hashtbl.set environment ~key:fst ~data:current_path;
     Hashtbl.set environment ~key:snd ~data:current_path
   | None -> ());
  match recurse.f state computation with
  | Fetch { id = v_id; _ } ->
    let uid = Type_equal.Id.uid v_id in
    (match Hashtbl.find environment uid with
     | None -> ()
     | Some named_id -> add_dag_relationship ~from:named_id ~to_:current_path);
    computation
  | c -> c
;;

let iter_graph_updates (t : (_, _, _, _) Computation.t) ~on_update =
  let graph_info = ref empty in
  let add_dag_relationship ~from ~to_ =
    let (lazy from), (lazy to_) = from, to_ in
    let gm = !graph_info in
    graph_info := { gm with dag = Map.add_multi gm.dag ~key:from ~data:to_ };
    on_update !graph_info
  in
  let add_tree_relationship ~from ~to_ ~from_info =
    let (lazy from), (lazy to_) = from, to_ in
    let gm = !graph_info in
    graph_info
    := { gm with
         info = Map.add_exn gm.info ~key:from ~data:from_info
       ; tree = Map.add_exn gm.tree ~key:from ~data:to_
       };
    on_update !graph_info
  in
  let environment = Type_equal.Id.Uid.Table.create () in
  Transform.map
    ~init:(environment, add_tree_relationship, add_dag_relationship)
    ~computation_mapper:{ f = computation_map }
    ~value_mapper:{ f = value_map }
    t
;;

let iter_graph_updates_packed (Computation.T t) ~on_update =
  Computation.T { t with t = iter_graph_updates ~on_update t.t }
;;
