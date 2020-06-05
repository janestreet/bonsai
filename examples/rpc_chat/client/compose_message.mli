open! Core_kernel
open! Async_kernel
open! Bonsai_web

val component
  :  send_message:(string -> unit Effect.t) Bonsai.Proc.Value.t
  -> Vdom.Node.t Bonsai.Proc.Computation.t
