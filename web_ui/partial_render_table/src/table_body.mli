open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing : sig
  type cell =
    { cell_focused : bool
    ; view : Vdom.Node.t
    }

  type row =
    { id : Opaque_map.Key.t
    ; row_focused : bool
    ; cells : cell list
    }

  type t =
    { column_names : Vdom.Node.t list list (** See [Header_tree.column_names]. *)
    ; rows : row list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

val component
  :  themed_attrs:Table_view.Themed.t Value.t
  -> key_comparator:('key, 'cmp) Bonsai.comparator
  -> column_id_comparator:('column_id, 'column_id_cmp) Bonsai.comparator
  -> row_height:[< `Px of int ] Value.t
  -> headers:'column_id Header_tree.t Value.t
  -> leaves:'column_id Header_tree.leaf list Value.t
  -> assoc:
       (('key * 'data) Opaque_map.t Value.t
        -> ('key * ('column_id * Vdom.Node.t) list) Opaque_map.t Computation.t)
  -> column_widths:('column_id, Column_size.t, 'column_id_cmp) Map.t Value.t
  -> visually_focused:('key, 'column_id, 'kind) Focus.focused Value.t
  -> on_cell_click:('key -> 'column_id -> unit Effect.t) Value.t
  -> extra_row_attrs:('key -> Vdom.Attr.t list) Value.t
  -> ('key, 'data) Collated.t Value.t
  -> ('key * 'data) Opaque_map.t Value.t
  -> (Table_view.Body.t * For_testing.t Lazy.t) Computation.t
