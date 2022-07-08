open! Core
open! Bonsai_web
open Incr_map_collate

module type S = sig
  type t
  type key
  type data

  val headers : t -> Header_tree.t Computation.t

  val instantiate_cells
    :  t
    -> (key, 'cmp) Bonsai.comparator
    -> (key * data) Map_list.t Value.t
    -> (key * Vdom.Node.t list) Map_list.t Computation.t
end

module type S_with_sorter = sig
  type t
  type key
  type data

  val headers_and_sorters
    :  t
    -> change_sort:([ `Add | `Replace ] -> int -> unit Vdom.Effect.t) Value.t
    -> sort_order:(int * [ `Asc | `Desc ]) list Value.t
    -> ((key * data -> key * data -> int) Int.Map.t * Header_tree.t) Computation.t

  val instantiate_cells
    :  t
    -> (key, 'cmp) Bonsai.comparator
    -> (key * data) Map_list.t Value.t
    -> (key * Vdom.Node.t list) Map_list.t Computation.t
end

type ('key, 'data) t =
  | T :
      { value : 'a
      ; vtable : (module S with type t = 'a and type key = 'key and type data = 'data)
      }
      -> ('key, 'data) t

type ('key, 'data) with_sorter =
  | Y :
      { value : 'a
      ; vtable :
          (module S_with_sorter with type t = 'a and type key = 'key and type data = 'data)
      }
      -> ('key, 'data) with_sorter
