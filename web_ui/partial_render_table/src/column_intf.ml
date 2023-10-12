open! Core
open! Bonsai_web
module Sort_kind = Bonsai_web_ui_partial_render_table_protocol.Sort_kind

module type Header_helpers = sig
  module Sort_state := Bonsai_web_ui_partial_render_table_protocol.Sort_state

  val legacy : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t
  val default : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t
end

module type S = sig
  type t
  type key
  type data

  val headers : t -> Header_tree.t Computation.t

  val instantiate_cells
    :  t
    -> (key, 'cmp) Bonsai.comparator
    -> (key * data) Opaque_map.t Value.t
    -> (key * Vdom.Node.t list) Opaque_map.t Computation.t
end

module type S_with_sorter = sig
  type t
  type key
  type data

  val headers_and_sorters
    :  t
    -> int Sortable_header.t Value.t
    -> ((key, data) Sort_kind.t Int.Map.t * Header_tree.t) Computation.t

  val instantiate_cells
    :  t
    -> (key, 'cmp) Bonsai.comparator
    -> (key * data) Opaque_map.t Value.t
    -> (key * Vdom.Node.t list) Opaque_map.t Computation.t
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
