open! Core
open! Async_kernel
open! Import

val component : (Rpgdice.Roll_spec.t * Vdom.Node.t) Computation.t
