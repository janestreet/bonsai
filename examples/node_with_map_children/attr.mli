open! Core
open! Bonsai_web.Cont

type t =
  { attr : Vdom.Attr.t
  ; reset : unit Effect.t
  }

val component : Bonsai.graph -> t Bonsai.t
