open! Core
open! Bonsai_web.Cont

val component
  :  send_message:(string -> unit Effect.t) Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
