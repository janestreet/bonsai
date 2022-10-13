open! Core
open! Bonsai_web
open! Import

(** This attribute, when added to a Vdom node adds the ability for that dom-node to be
    clicked-and-dragged to change its parents width. *)
val attr : Vdom.Attr.t


