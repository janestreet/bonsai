open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing : sig
  type cell =
    { id : Int63.t
    ; selected : bool
    ; view : Vdom.Node.t list
    }

  type t =
    { column_names : Vdom.Node.t list
    ; cells : cell list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

val component
  :  comparator:('key, 'cmp) Bonsai.comparator
  -> row_height:[< `Px of int ]
  -> leaves:Header_tree.leaf list Value.t
  -> assoc:
       (('key, Int63.t * 'data, 'cmp) Map.t Value.t
        -> ('key, Int63.t * Vdom.Node.t list, 'cmp) Map.t Computation.t)
  -> column_widths:(int, [< `Px of float ], 'c) Map.t Value.t
  -> visually_focused:'key option Value.t
  -> row_click_handler:('key -> unit Effect.t) Value.t option
  -> ('key, 'data) Collated.t Value.t
  -> ('key, Int63.t * 'data, 'cmp) Map.t Value.t
  -> (Vdom.Node.t list * For_testing.t Lazy.t) Computation.t
