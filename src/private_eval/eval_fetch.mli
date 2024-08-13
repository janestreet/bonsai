open! Core
open! Import

val f
  :  id:'a Type_equal.Id.t
  -> default:'b
  -> for_some:('a -> 'b)
  -> ('b, unit) Computation.packed_info Trampoline.t
