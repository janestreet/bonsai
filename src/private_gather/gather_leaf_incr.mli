open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  input:'a Value.t
  -> compute:('b -> 'a Incr.t -> 'c Incr.t)
  -> time_source:'b
  -> here:Source_code_position.t
  -> ('c, unit) Computation.packed_info Trampoline.t
