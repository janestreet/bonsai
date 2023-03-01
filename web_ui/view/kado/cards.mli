open! Core
open! Import

val make
  :  View.Constants.t
  -> container_attr:Vdom.Attr.t
  -> title_attr:Vdom.Attr.t
  -> content_attr:Vdom.Attr.t
  -> intent:Intent.t option
  -> on_click:unit Effect.t
  -> title:Vdom.Node.t list
  -> title_kind:Card_title_kind.t
  -> content:Vdom.Node.t list
  -> Vdom.Node.t
