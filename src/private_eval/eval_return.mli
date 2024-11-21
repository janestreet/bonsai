open! Core
open! Import

val f
  :  value:'a Value.t
  -> here:Source_code_position.t
  -> ('a, unit) Computation.packed_info Trampoline.t
