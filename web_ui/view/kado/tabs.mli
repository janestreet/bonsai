open! Core
open! Import

val make
  :  attrs:Vdom.Attr.t list
  -> per_tab_attrs:('a -> is_active:bool -> Vdom.Attr.t list)
  -> on_change:(from:'a -> to_:'a -> unit Effect.t)
  -> equal:('a -> 'a -> bool)
  -> active:'a
  -> ('a * Vdom.Node.t) list
  -> Vdom.Node.t
