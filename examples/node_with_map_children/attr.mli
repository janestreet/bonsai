open! Core
open! Bonsai_web

type t =
  { attr : Vdom.Attr.t
  ; reset : unit Effect.t
  }

val component : t Computation.t
