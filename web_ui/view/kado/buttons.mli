open! Core
open! Import

val make
  :  View.Constants.t
  -> attr:Vdom.Attr.t
  -> disabled:bool
  -> intent:Intent.t option
  -> tooltip:string option
  -> on_click:unit Ui_effect.t
  -> Vdom.Node.t list
  -> Vdom.Node.t
