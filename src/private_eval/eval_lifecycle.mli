open! Core
open! Import

val f
  :  lifecycle:Lifecycle.t option Value.t
  -> (unit, unit) Computation.packed_info Trampoline.t
