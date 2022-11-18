open! Core
open! Import

module Direction : sig
  type t =
    | Top
    | Right
    | Bottom
    | Left
end

val make
  :  Constants.t
  -> container_attr:Vdom.Attr.t
  -> tooltip_attr:Vdom.Attr.t
  -> direction:Direction.t
  -> tipped:Vdom.Node.t
  -> tooltip:Vdom.Node.t
  -> Vdom.Node.t
