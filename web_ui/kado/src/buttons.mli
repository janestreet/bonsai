open! Core
open! Import

val make
  :  View.Constants.t
  -> attrs:Vdom.Attr.t list
  -> disabled:bool
  -> intent:Intent.t option
  -> tooltip:string option
  -> on_click:unit Ui_effect.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

val vertical_group : Vdom.Attr.t
val horizontal_group : Vdom.Attr.t
val small : Vdom.Attr.t
val thinking : Vdom.Attr.t
val pressed : Vdom.Attr.t
