(** This module provides functionality of ordering in the PRT. This means that a user
    can decorate labels before column creation using the [component] function and get
    ordering from the [Result], which in it's turn can be used for sorting the rows on
    the server side. *)

open! Core
open Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t

(** Adds a mouse on-click handler on a [Vdom.Node] with a specified column index and an
    icon from {Icons} module to the left of the node. The mouse click event updates the
    ordering and changes the icon. Just a mouse click sets sorting by a single column,
    Shift-mouse click adds sorting by a column (so that multiple columns are used).
*)
val decorate : 'col_id t -> Vdom.Node.t -> 'col_id -> Vdom.Node.t

val order : 'col_id t -> 'col_id Order.t

val component
  :  ?initial_order:'col_id Order.t Value.t
  -> (module Col_id with type t = 'col_id)
  -> 'col_id t Computation.t
