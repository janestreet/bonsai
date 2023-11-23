open! Core
open! Bonsai_web
module Sort_kind = Bonsai_web_ui_partial_render_table_protocol.Sort_kind

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
  type column_id
  type column_id_cmp

  val headers_and_sorters
    :  t
    -> column_id Sortable.t Value.t
    -> ((column_id, (key, data) Sort_kind.t, column_id_cmp) Map.t * Header_tree.t)
       Computation.t

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

type ('key, 'data, 'column_id) with_sorter =
  | Y :
      { value : 'a
      ; vtable :
          (module S_with_sorter
             with type t = 'a
              and type key = 'key
              and type data = 'data
              and type column_id = 'column_id
              and type column_id_cmp = 'column_id_cmp)
      ; col_id : ('column_id, 'column_id_cmp) Bonsai.comparator
      }
      -> ('key, 'data, 'column_id) with_sorter
