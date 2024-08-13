open! Core
open! Import

val f
  :  input:'a Value.t
  -> compute:('b -> 'a Incr.t -> 'c Incr.t)
  -> time_source:'b
  -> ('c, unit) Computation.packed_info Trampoline.t
