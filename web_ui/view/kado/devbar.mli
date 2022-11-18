open! Core
open! Import

val make
  :  View.Constants.t
  -> attr:Vdom.Attr.t
  -> count:int
  -> intent:Intent.t option
  -> string
  -> Vdom.Node.t
