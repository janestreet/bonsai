open! Core
open! Import

val make
  :  View.Constants.t
  -> container_attrs:Vdom.Attr.t list
  -> title_attrs:Vdom.Attr.t list
  -> content_attrs:Vdom.Attr.t list
  -> intent:Intent.t option
  -> on_click:unit Effect.t
  -> title:Vdom.Node.t list
  -> title_kind:Card_title_kind.t
  -> content:Vdom.Node.t list
  -> Vdom.Node.t
