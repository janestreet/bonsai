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
  -> container_attrs:Vdom.Attr.t list
  -> tooltip_attrs:Vdom.Attr.t list
  -> direction:Direction.t
  -> tipped:Vdom.Node.t
  -> tooltip:Vdom.Node.t
  -> Vdom.Node.t
