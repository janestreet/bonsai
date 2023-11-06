open! Core
open! Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol

(** This module allows you to create a state machine for column sorting order, get/update
    the current value, and create header labels via the [Header] submodule's utils. *)

type 'col_id t

val state
  :  ?initial_order:'col_id Order.t Value.t
  -> (module Col_id with type t = 'col_id)
  -> 'col_id t Computation.t

val order : 'col_id t -> 'col_id Order.t
val inject : 'col_id t -> 'col_id Order.Action.t -> unit Effect.t

module Header : sig
  (** The [Header] module contains utils for displaying / controlling sort state from
      column headers. *)

  (** Wraps the input label in an HTML element, which also contains an icon that reflects
      the current sort state. *)
  val with_icon : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t

  module Expert : sig
    (** If using [Partial_render_table.Basic], this is done for you. Adds a mouse on-click
        handler on a [Vdom.Node] with a specified column index and an icon from {Icons}
        module to the left of the node. The mouse click event updates the ordering and
        changes the icon. Just a mouse click sets sorting by a single column, Shift-mouse
        click adds sorting by a column (so that multiple columns are used). *)
    val default_click_handler
      :  'col_id t
      -> column_id:'col_id
      -> sortable:bool
      -> (Sort_state.t -> Vdom.Node.t)
      -> Vdom.Node.t
  end

  module Legacy : sig
    val wrap_with_icon : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t
  end
end
