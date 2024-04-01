open! Core
open! Bonsai_web.Cont

type t =
  { tag : string
  ; reset : unit Effect.t
  }

val component : Bonsai.graph -> t Bonsai.t
