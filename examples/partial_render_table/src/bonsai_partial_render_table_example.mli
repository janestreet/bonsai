open! Core
open! Bonsai_web
module Row = Row

type t =
  { table : Vdom.Node.t
  ; focus_attr : Vdom.Attr.t
  ; set_column_width :
      column_id:Bonsai_web_ui_partial_render_table.Indexed_column_id.t
      -> [ `Px_float of float ]
      -> unit Ui_effect.t
  }

val component
  :  ?filter:(key:string -> data:Row.t -> bool) Value.t
  -> focus_kind:[ `Row | `Cell ]
  -> row_height:[ `Px of int ] Value.t
  -> theming:[ `Legacy_don't_use_theme | `Themed ]
  -> should_show_position:bool Value.t
  -> (string, Row.t, Base.String.comparator_witness) Base.Map.t Value.t
  -> t Computation.t

module Layout_form : sig
  module Params : sig
    type t =
      { themed : bool
      ; show_position : bool
      ; cell_based_highlighting : bool
      ; row_height : [ `Px of int ]
      ; num_rows : int
      }
  end

  val component : (Vdom.Node.t * Params.t) Computation.t
end

module Column_width_form : sig
  val component
    :  set_column_width:
         (column_id:Bonsai_web_ui_partial_render_table.Indexed_column_id.t
          -> [ `Px_float of float ]
          -> unit Ui_effect.t)
         Value.t
    -> Vdom.Node.t Computation.t
end
