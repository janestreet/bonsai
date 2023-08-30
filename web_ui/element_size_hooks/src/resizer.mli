open! Core
open! Bonsai_web
open! Import

module Side : sig
  type t =
    | Left
    | Right
end

(** This attribute, when added to a Vdom node adds the ability for that dom-node to be
    clicked-and-dragged to change its parents width. *)
val attr : side:Side.t -> Vdom.Attr.t
