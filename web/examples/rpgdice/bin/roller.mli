open! Core_kernel
open! Async_kernel
open! Import

include
  Bonsai.S
  with type Input.t = Rpgdice.Roll_spec.t Or_error.t
  with type Result.t = Vdom.Node.t

val initial_model : Model.t
