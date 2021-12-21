open! Core
open! Bonsai_web

val component
  :  send_message:(string -> unit Effect.t) Value.t
  -> Vdom.Node.t Computation.t
