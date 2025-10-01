open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  value:'a Value.t
  -> here:Source_code_position.t
  -> ('a, unit) Computation.packed_info Trampoline.t
