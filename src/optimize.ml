open! Core_kernel
open! Import
open Component
open Composition_infix

(* Notes:
   - map_input being merged into a leaf node is _not_ implemented because map_input
     frequently decreases the amount of incremental recomputation, e.g., by making the new
     input a small part of the old one.
   - project_model being merged into a leaf node is _not_ implemented for the same reasons
     as above. *)

let map_over_constant = function
  | Packed.T (Mapn.Map1 { t = Const.C x; f }, typ_id) -> Packed.T (Const.C (f x), typ_id)
  | other -> other
;;

let map_over_map = function
  | Packed.T (Mapn.Map1 { t = Mapn.Map1 { t; f = f2 }; f = f1 }, typ_id) ->
    Packed.T (Mapn.Map1 { t; f = f2 >> f1 }, typ_id)
  | other -> other
;;

let map_over_map2 = function
  | Packed.T
      ( Mapn.Map1
          { f = f1; t = Mapn.Map2 { t1; action_type_id1; t2; action_type_id2; f = f2 } }
      , typ_id ) ->
    Packed.T
      ( Mapn.Map2
          { t1; t2; action_type_id1; action_type_id2; f = (fun x y -> f1 (f2 x y)) }
      , typ_id )
  | other -> other
;;

let map_over_leaf = function
  | Packed.T (Mapn.Map1 { t = Leaf.C { apply_action; compute; name }; f }, typ_id) ->
    let compute ~inject input model = compute ~inject input model |> f in
    Packed.T (Leaf.C { apply_action; compute; name }, typ_id)
  | other -> other
;;

let map_input_over_map_input = function
  | Packed.T (Map_input.C { t = Map_input.C { t; f = f1 }; f = f2 }, typ_id) ->
    Packed.T (Map_input.C { t; f = f2 >> f1 }, typ_id)
  | other -> other
;;

let project_over_project = function
  | Packed.T
      ( Projection.C
          { t = Projection.C { t; projection = project_2 }; projection = project_1 }
      , typ_id ) ->
    Packed.T
      (Projection.C { t; projection = Projection.compose project_1 project_2 }, typ_id)
  | other -> other
;;

let compose_over_pure = function
  | Packed.T (Compose.C { t1 = Const.C c; t2 = Pure.Pure_input f; _ }, _) ->
    Packed.T (Const.C (f c), nothing_type_id)
  | Packed.T (Compose.C { t1 = Pure.Pure_input g; t2 = Pure.Pure_input f; _ }, _) ->
    Packed.T (Pure.Pure_input (g >> f), nothing_type_id)
  | Packed.T (Compose.C { t1 = Pure.Pure_input f; t2 = t; action_type_id2; _ }, _) ->
    Packed.T (Map_input.C { t; f }, action_type_id2)
  | Packed.T (Compose.C { t1 = t; t2 = Pure.Pure_input f; action_type_id1; _ }, _) ->
    Packed.T (Mapn.Map1 { t; f }, action_type_id1)
  | other -> other
;;

let optimize component =
  let visit node =
    node
    |> map_over_constant
    |> map_over_map
    |> map_over_map2
    |> map_over_leaf
    |> map_input_over_map_input
    |> project_over_project
    |> compose_over_pure
  in
  visit_ext component ({ visit } : Visitor.t)
;;
