open! Core
open! Bonsai_web

type t

val leaf : Vdom.Node.t -> t
val branch : Vdom.Node.t -> t list -> t
val to_vdom : t -> Vdom.Node.t
