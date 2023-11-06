open! Core
open! Import

val make
  :  View.Constants.t
  -> is_dark:bool
  -> attrs:Vdom.Attr.t list
  -> count:int
  -> intent:Intent.t option
  -> string
  -> Vdom.Node.t
