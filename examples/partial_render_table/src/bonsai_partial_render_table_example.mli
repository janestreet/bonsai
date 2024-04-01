open! Core
open! Bonsai_web.Cont
module Row = Row

type t =
  { table : Vdom.Node.t
  ; focus_attr : Vdom.Attr.t
  ; set_column_width :
      column_id:Bonsai_web_ui_partial_render_table.Indexed_column_id.t
      -> [ `Px_float of float ]
      -> unit Ui_effect.t
  ; lock_focus : unit Ui_effect.t
  ; unlock_focus : unit Ui_effect.t
  ; focus_is_locked : bool
  }

val component
  :  ?filter:(key:string -> data:Row.t -> bool) Bonsai.t
  -> focus_kind:[ `Row | `Cell ]
  -> row_height:[ `Px of int ] Bonsai.t
  -> theming:[ `Legacy_don't_use_theme | `Themed ]
  -> multisort_columns_when:[ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click ] Bonsai.t
  -> should_show_position:bool Bonsai.t
  -> (string, Row.t, Base.String.comparator_witness) Base.Map.t Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t

module Layout_form : sig
  module Params : sig
    type t =
      { themed : bool
      ; show_position : bool
      ; cell_based_highlighting : bool
      ; row_height : [ `Px of int ]
      ; num_rows : int
      ; multisort_columns_when : [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click ]
      }
  end

  val component : Bonsai.graph -> (Vdom.Node.t * Params.t) Bonsai.t
end

module Column_width_form : sig
  val component
    :  set_column_width:
         (column_id:Bonsai_web_ui_partial_render_table.Indexed_column_id.t
          -> [ `Px_float of float ]
          -> unit Ui_effect.t)
         Bonsai.t
    -> Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end
