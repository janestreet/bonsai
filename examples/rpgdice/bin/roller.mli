open! Core_kernel
open! Async_kernel
open! Import

val component
  :  Rpgdice.Roll_spec.t Or_error.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
