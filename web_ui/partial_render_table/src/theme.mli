open! Core
open! Bonsai_web

module Cell : sig
  module Col_styles : sig
    type t

    val create
      :  row_height:int
      -> col_widths:[< `Hidden of float | `Visible of float ] list
      -> cols_visible:bool list
      -> int
      -> t
  end

  type t =
    { col_styles : Col_styles.t
    ; content : Vdom.Node.t
    }

  val view : t -> Vdom.Node.t
end

module Row : sig
  module Styles : sig
    type t

    val create : row_height:int -> row_width:float -> t
  end

  val view
    :  styles:Styles.t
    -> is_selected:bool
    -> on_row_click:unit Ui_effect.t
    -> col_styles:(int -> Cell.Col_styles.t)
    -> cell_contents:Vdom.Node.t list
    -> Vdom.Node.t
end

module Body : sig
  val view
    :  padding_top:int
    -> padding_bottom:int
    -> rows:Vdom.Node.t Opaque_map.Key.Map.t
    -> Vdom.Node.t
end
