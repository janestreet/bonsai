open Core.Core_stable

module Stable = struct
  module Node_info = struct
    module V1 = struct
      type t =
        { node_type : String.V1.t
        ; here : Core.Source_code_position.Stable.V1.t Option.V1.t
        }
      [@@deriving sexp, bin_io, compare]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 127c33aa3895110d2fdd1b3c356bf5c3 |}]
      ;;
    end

    module V2 = struct
      type t =
        { node_type : String.V1.t
        ; here : Core.Source_code_position.Stable.V1.t Option.V1.t
        ; id : Int.V1.t
        }
      [@@deriving sexp, compare, bin_io, stable_record ~version:V1.t ~remove:[ id ]]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| da0e8001d1f5692d5a56c26e18c81a01 |}]
      ;;

      let to_v1 t = to_V1_t t
      let of_v1 t = of_V1_t t ~id:(-1)
    end

    module V3 = struct
      type t = V1.t [@@deriving sexp, compare, bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 127c33aa3895110d2fdd1b3c356bf5c3 |}]
      ;;

      let to_v2 = V2.of_v1
      let of_v2 = V2.to_v1
    end
  end

  module V1 = struct
    type t =
      { tree : Node_path.Stable.V1.t Node_path.Stable.V1.Map.t
      ; dag : Node_path.Stable.V1.t list Node_path.Stable.V1.Map.t
      ; info : Node_info.V1.t Node_path.Stable.V1.Map.t
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 122924cb491f2c00727ae98b5c47f343 |}]
    ;;
  end

  module V2 = struct
    type t =
      { tree : Node_path.Stable.V1.t Node_path.Stable.V1.Map.t
      ; dag : Node_path.Stable.V1.t list Node_path.Stable.V1.Map.t
      ; info : Node_info.V2.t Node_path.Stable.V1.Map.t
      }
    [@@deriving bin_io, compare, sexp, stable_record ~version:V1.t ~modify:[ info ]]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ca80280d86a032bbd663626d8558fe6f |}]
    ;;

    let of_v1 t =
      let open Core in
      of_V1_t t ~modify_info:(Map.map ~f:Node_info.V2.of_v1)
    ;;

    let to_v1 t =
      let open Core in
      to_V1_t t ~modify_info:(Map.map ~f:Node_info.V2.to_v1)
    ;;
  end

  module V3 = struct
    type t =
      { tree : Node_path.Stable.V1.t Node_path.Stable.V1.Map.t
      ; dag : Node_path.Stable.V1.t list Node_path.Stable.V1.Map.t
      ; info : Node_info.V3.t Node_path.Stable.V1.Map.t
      }
    [@@deriving bin_io, compare, sexp, stable_record ~version:V2.t ~modify:[ info ]]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 122924cb491f2c00727ae98b5c47f343 |}]
    ;;

    let of_v2 t =
      let open Core in
      of_V2_t t ~modify_info:(Map.map ~f:Node_info.V3.of_v2)
    ;;

    let to_v2 t =
      let open Core in
      to_V2_t t ~modify_info:(Map.map ~f:Node_info.V3.to_v2)
    ;;
  end
end

open Stable
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
  [@@deriving sexp, bin_io, equal]
end

module Node_info = struct
  type t = Node_info.V1.t =
    { node_type : string
    ; here : Source_code_position.t option
    }
  [@@deriving sexp, bin_io, equal]

  let of_value (type a) ({ value; here } : a Value.t) =
    let node_type =
      match value with
      | Constant _ -> "constant"
      | Exception _ -> "exception"
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
    { node_type; here = Some here }
  ;;

  let of_computation (type result) (computation : result Computation.t) =
    let here = Computation.source_code_position computation in
    let node_type =
      match computation with
      | Return _ -> "return"
      | Leaf1 _ -> "leaf1"
      | Leaf0 _ -> "leaf0"
      | Leaf_incr _ -> "leaf_incr"
      | Sub _ -> "sub"
      | Store _ -> "store"
      | Fetch _ -> "fetch"
      | Assoc _ -> "assoc"
      | Assoc_on _ -> "assoc_on"
      | Assoc_simpl _ -> "assoc_simpl"
      | Switch _ -> "switch"
      | Lazy _ -> "lazy"
      | Fix_define _ -> "fix_define"
      | Fix_recurse _ -> "fix_recurse"
      | Wrap _ -> "wrap"
      | With_model_resetter _ -> "with_model_resetter"
      | Path _ -> "path"
      | Lifecycle _ -> "lifecycle"
      | Computation_watcher _ -> "computation_watcher"
    in
    { node_type; here = Some here }
  ;;
end

type t = V3.t =
  { tree : Node_path.t Node_path.Map.t
  ; dag : Node_path.t list Node_path.Map.t
  ; info : Node_info.t Node_path.Map.t
  }
[@@deriving bin_io, sexp, equal]

let empty =
  { tree = Node_path.Map.empty; dag = Node_path.Map.empty; info = Node_path.Map.empty }
;;

let already_printed_bug_message = ref false

let value_map
  (type a)
  ({ recurse; var_from_parent; parent_path; current_path; _ } :
    _ Transform.For_value.context)
  state
  (value : a Value.t)
  =
  let environment, add_tree_relationship, add_dag_relationship = state in
  let node_info = Node_info.of_value value in
  (match var_from_parent with
   | Transform.Var_from_parent.One var_from_parent ->
     Hashtbl.set environment ~key:var_from_parent ~data:current_path
   | Two (fst, snd) ->
     Hashtbl.set environment ~key:fst ~data:current_path;
     Hashtbl.set environment ~key:snd ~data:current_path
   | None -> ());
  add_tree_relationship ~from:current_path ~to_:parent_path ~from_info:node_info;
  add_dag_relationship ~from:current_path ~to_:parent_path;
  let () =
    match value.value with
    | Named (_, id) ->
      (match Hashtbl.find environment (Type_equal.Id.uid id) with
       | Some named_id -> add_dag_relationship ~from:named_id ~to_:current_path
       | None when !already_printed_bug_message -> ()
       | None ->
         already_printed_bug_message := true;
         print_s [%message "BUG" [%here]])
    | _ -> ()
  in
  recurse state value
;;

let computation_map
  (type result)
  ({ recurse; var_from_parent; parent_path; current_path } :
    _ Transform.For_computation.context)
  state
  (computation : result Computation.t)
  : result Computation.t Trampoline.t
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
  let open Trampoline.Let_syntax in
  let%bind recursed = recurse state computation in
  match recursed with
  | Fetch { id = v_id; _ } ->
    let uid = Type_equal.Id.uid v_id in
    (match Hashtbl.find environment uid with
     | None -> ()
     | Some named_id -> add_dag_relationship ~from:named_id ~to_:current_path);
    return computation
  | _ -> return recursed
;;

let iter_graph_updates (t : _ Computation.t) ~on_update =
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
    let info =
      match Map.add gm.info ~key:from ~data:from_info with
      | `Ok info -> info
      | `Duplicate ->
        print_s
          [%message
            "BUG: [ duplicate info ]" (from : Node_path.t) (from_info : Node_info.t)];
        gm.info
    in
    let tree =
      match Map.add gm.tree ~key:from ~data:to_ with
      | `Ok tree -> tree
      | `Duplicate ->
        print_s
          [%message "BUG: [ duplicate tree ]" (from : Node_path.t) (to_ : Node_path.t)];
        gm.tree
    in
    graph_info := { gm with info; tree };
    on_update !graph_info
  in
  let environment = Type_equal.Id.Uid.Table.create () in
  Transform.map
    ~init:(environment, add_tree_relationship, add_dag_relationship)
    ~computation_mapper:{ f = computation_map }
    ~value_mapper:{ f = value_map }
    t
;;

(* A lot of nodes in the bonsai graph don't actually have source-code locations attached.
   This functions walks up the graph from each node and attaches the location from the
   nearest parent. *)
let pull_source_locations_from_nearest_parent t =
  let info = ref t.info in
  let rec find_and_update_nearest_here key : Source_code_position.t option =
    let%bind.Option { Node_info.node_type; here } = Map.find !info key in
    let here =
      match here with
      | Some here -> Some here
      | None ->
        let%bind.Option parent = Map.find t.tree key in
        let%map.Option here = find_and_update_nearest_here parent in
        { here with pos_fname = "~" ^ here.pos_fname }
    in
    info := Map.set !info ~key ~data:{ node_type; here };
    here
  in
  let update_nearest_here key =
    let (_ : Source_code_position.t option) = find_and_update_nearest_here key in
    ()
  in
  Map.iter_keys !info ~f:update_nearest_here;
  !info
;;
