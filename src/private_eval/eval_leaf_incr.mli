open! Core
open! Import

val f
  :  input:'a Value.t
  -> compute:('b -> 'a Incr.t -> 'c Incr.t)
  -> time_source:'b
  -> here:Source_code_position.t
  -> ('c, unit) Computation.packed_info Trampoline.t
