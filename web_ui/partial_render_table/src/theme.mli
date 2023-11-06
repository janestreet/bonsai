open! Core
open! Bonsai_web

module Header_label : sig
  val wrap_clickable
    :  sortable:bool
    -> handle_click:Vdom.Attr.t
    -> Vdom.Node.t
    -> Vdom.Node.t

  val wrap_with_icon
    :  Vdom.Node.t
    -> Bonsai_web_ui_partial_render_table_protocol.Sort_state.t
    -> Vdom.Node.t
end

module Header : sig
  module Header_cell : sig
    type t

    val leaf_view
      :  column_width:Css_gen.Length.t
      -> set_column_width:([> `Px_float of float ] -> unit Ui_effect.t)
      -> visible:bool
      -> label:Vdom.Node.t
      -> unit
      -> t

    val spacer_view : colspan:int -> unit -> t
    val group_view : colspan:int -> label:Vdom.Node.t -> unit -> t
  end

  module Header_row : sig
    type t

    val view : Header_cell.t list -> t
  end

  type t

  val view
    :  set_header_client_rect:
         (Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.t -> unit Ui_effect.t)
    -> Header_row.t list
    -> t
end

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

  type t

  val view : col_styles:Col_styles.t -> Vdom.Node.t -> t
  val empty_content : Vdom.Node.t
end

module Row : sig
  module Styles : sig
    type t

    val create : row_height:int -> row_width:float -> t
  end

  type t

  val view
    :  styles:Styles.t
    -> is_selected:bool
    -> on_row_click:unit Ui_effect.t
    -> Cell.t list
    -> t
end

module Body : sig
  type t

  val view : padding_top:int -> padding_bottom:int -> rows:Row.t Opaque_map.Key.Map.t -> t
end

module Table : sig
  val view
    :  private_body_classname:string
    -> vis_change_attr:Vdom.Attr.t
    -> total_height:int
    -> Header.t
    -> Body.t
    -> Vdom.Node.t
end
