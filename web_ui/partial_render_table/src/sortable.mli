open! Core
open! Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol

(** This module allows you to create a state machine for column sorting order, get/update
    the current value, and create header labels via the [Header] submodule's utils. *)

type 'col_id t

val state
  :  ?initial_order:'col_id Order.t Value.t
  -> equal:('col_id -> 'col_id -> bool)
  -> unit
  -> 'col_id t Computation.t

val order : 'col_id t -> 'col_id Order.t
val inject : 'col_id t -> 'col_id Order.Action.t -> unit Effect.t

module Header : sig
  (** The [Header] module contains utils for displaying / controlling sort state from
      column headers. *)

  (** Wraps the input label in an HTML element, which also contains an icon that reflects
      the current sort state. *)
  val with_icon
    :  ?sort_indicator_attrs:Vdom.Attr.t list
    -> Vdom.Node.t
    -> Sort_state.t
    -> Vdom.Node.t

  module Expert : sig
    (** If using [Partial_render_table.Basic], this is done for you. Adds a mouse on-click
        handler on a [Vdom.Node] with a specified column index and an icon from {Icons}
        module to the left of the node. The mouse click event updates the ordering and
        changes the icon.

        If the [multisort_columns_when] key combo is used, new columns are added to the
        sort order, instead of replacing it. Defaults to [`Shift_click]. *)
    val default_click_handler
      :  ?multisort_columns_when:[ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click ]
      -> 'col_id t
      -> column_id:'col_id
      -> sortable:bool
      -> (Sort_state.t -> Vdom.Node.t)
      -> Vdom.Node.t
  end

  module Legacy : sig
    val wrap_with_icon : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t
  end
end
