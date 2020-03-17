open! Core_kernel
open! Async_kernel
open! Import

include
  Bonsai.S
  with type Input.t = unit
  with type Result.t = Rpgdice.Roll_spec.t * Vdom.Node.t

val initial_model : Model.t
