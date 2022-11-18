open! Core
open! Import

module C = struct
  class type t =
    object
      method theme_name : string
      method constants : Constants.t

      method button :
        attr:Vdom.Attr.t
        -> disabled:bool
        -> intent:Constants.Intent.t option
        -> tooltip:string option
        -> on_click:unit Effect.t
        -> Vdom.Node.t list
        -> Vdom.Node.t

      method tabs :
        attr:Vdom.Attr.t
        -> per_tab_attr:('a -> is_active:bool -> Vdom.Attr.t)
        -> on_change:(from:'a -> to_:'a -> unit Effect.t)
        -> equal:('a -> 'a -> bool)
        -> active:'a
        -> ('a * Vdom.Node.t) list
        -> Vdom.Node.t

      method devbar :
        attr:Vdom.Attr.t
        -> count:int
        -> intent:Constants.Intent.t option
        -> string
        -> Vdom.Node.t

      method humanize_sexp : Sexp.t -> string
      method use_intent_fg_or_bg_for_highlighting : [ `Fg | `Bg ]

      method themed_text :
        attr:Vdom.Attr.t -> intent:Constants.Intent.t option -> string -> Vdom.Node.t

      method tooltip :
        container_attr:Vdom.Attr.t
        -> tooltip_attr:Vdom.Attr.t
        -> direction:Tooltip.Direction.t
        -> tipped:Vdom.Node.t
        -> tooltip:Vdom.Node.t
        -> Vdom.Node.t

      method app_attr : Vdom.Attr.t
      method codemirror_theme : For_codemirror.Theme.t option

      (* tables *)
      method table : Vdom.Attr.t
      method table_header : Vdom.Attr.t
      method table_header_row : Vdom.Attr.t
      method table_header_cell : Vdom.Attr.t
      method table_body : Vdom.Attr.t
      method table_body_row : Vdom.Attr.t
      method table_body_cell : Vdom.Attr.t
      method table_body_cell_empty : Vdom.Attr.t
    end
end

module type S = sig
  class c : C.t
end

type t = (module S)
