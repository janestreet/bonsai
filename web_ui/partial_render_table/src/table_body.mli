open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing : sig
  type cell =
    { id : Map_list.Key.t
    ; selected : bool
    ; view : Vdom.Node.t list
    }

  type t =
    { column_names : Vdom.Node.t list list (** See [Header_tree.column_names]. *)
    ; cells : cell list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

val component
  :  comparator:('key, 'cmp) Bonsai.comparator
  -> row_height:[< `Px of int ] Value.t
  -> leaves:Header_tree.leaf list Value.t
  -> headers:Header_tree.t Value.t
  -> assoc:
       (('key * 'data) Map_list.t Value.t
        -> ('key * Vdom.Node.t list) Map_list.t Computation.t)
  -> column_widths:Column_size.t Int.Map.t Value.t
  -> visually_focused:'key option Value.t
  -> on_row_click:('key -> unit Effect.t) Value.t
  -> ('key, 'data) Collated.t Value.t
  -> ('key * 'data) Map_list.t Value.t
  -> (Vdom.Node.t * For_testing.t Lazy.t) Computation.t
