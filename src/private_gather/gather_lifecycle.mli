open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  lifecycle:Lifecycle.t option Value.t
  -> here:Source_code_position.t
  -> (unit, unit) Computation.packed_info Trampoline.t
