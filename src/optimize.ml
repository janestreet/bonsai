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
  | Packed.T { unpacked = Mapn.Map1 { t = Const.C x; f }; action_type_id; model } ->
    Packed.T { unpacked = Const.C (f x); action_type_id; model }
  | other -> other
;;

let map_over_map = function
  | Packed.T
      { unpacked = Mapn.Map1 { t = Mapn.Map1 { t; f = f2 }; f = f1 }
      ; action_type_id
      ; model
      } -> Packed.T { unpacked = Mapn.Map1 { t; f = f2 >> f1 }; action_type_id; model }
  | other -> other
;;

let map_over_map2 = function
  | Packed.T
      { unpacked =
          Mapn.Map1
            { f = f1
            ; t =
                Mapn.Map2
                  { t1; action_type_id1; t2; action_type_id2; f = f2; model1; model2 }
            }
      ; action_type_id
      ; model
      } ->
    Packed.T
      { unpacked =
          Mapn.Map2
            { t1
            ; t2
            ; action_type_id1
            ; action_type_id2
            ; f = (fun x y -> f1 (f2 x y))
            ; model1
            ; model2
            }
      ; action_type_id
      ; model
      }
  | other -> other
;;

let map_over_leaf = function
  | Packed.T
      { unpacked = Mapn.Map1 { t = Leaf.C { apply_action; compute; name }; f }
      ; action_type_id
      ; model
      } ->
    let compute ~inject input model = compute ~inject input model |> f in
    Packed.T { unpacked = Leaf.C { apply_action; compute; name }; action_type_id; model }
  | other -> other
;;

let map_input_over_map_input = function
  | Packed.T
      { unpacked = Map_input.C { t = Map_input.C { t; f = f1 }; f = f2 }
      ; action_type_id
      ; model
      } -> Packed.T { unpacked = Map_input.C { t; f = f2 >> f1 }; action_type_id; model }
  | other -> other
;;

let compose_over_pure = function
  | Packed.T { unpacked = Compose.C { t1 = Const.C c; t2 = Pure.Pure_input f; _ }; _ } ->
    Packed.T
      { unpacked = Const.C (f c)
      ; action_type_id = nothing_type_id
      ; model = Packed.unit_model_info
      }
  | Packed.T
      { unpacked = Compose.C { t1 = Pure.Pure_input g; t2 = Pure.Pure_input f; _ }; _ }
    ->
    Packed.T
      { unpacked = Pure.Pure_input (g >> f)
      ; action_type_id = nothing_type_id
      ; model = Packed.unit_model_info
      }
  | Packed.T
      { unpacked =
          Compose.C { t1 = Pure.Pure_input f; t2 = t; action_type_id2; model2; _ }
      ; _
      } ->
    Packed.T
      { unpacked = Map_input.C { t; f }
      ; action_type_id = action_type_id2
      ; model = model2
      }
  | Packed.T
      { unpacked =
          Compose.C { t1 = t; t2 = Pure.Pure_input f; action_type_id1; model1; _ }
      ; _
      } ->
    Packed.T
      { unpacked = Mapn.Map1 { t; f }; action_type_id = action_type_id1; model = model1 }
  | other -> other
;;

let compose_into_return_input (type input result)
  : (input, result, 'incr, 'event) Packed.t -> (input, result, 'incr, 'event) Packed.t
  = function
    | Packed.T
        { unpacked = Compose.C { t1; t2 = Pure.Return_input; action_type_id1; model1; _ }
        ; _
        } -> Packed.T { unpacked = t1; action_type_id = action_type_id1; model = model1 }
    | other -> other
;;

let writer_over_reader (type input result)
  : (input, result, 'incr, 'event) Packed.t -> (input, result, 'incr, 'event) Packed.t
  = function
    | Packed.T
        { unpacked = Proc.Abstraction { type_id = w; t = Proc.Var { type_id = r } }; _ } as
      t ->
      (match Type_equal.Id.same_witness w r with
       | Some T -> Pure.input
       | None -> t)
    | other -> other
;;

let optimize component =
  let visit node =
    node
    |> writer_over_reader
    |> compose_into_return_input
    |> compose_over_pure
    |> map_over_constant
    |> map_over_map
    |> map_over_map2
    |> map_over_leaf
    |> map_input_over_map_input
  in
  visit_ext component ({ visit } : Visitor.t)
;;
