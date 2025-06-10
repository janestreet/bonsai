open! Core
open! Import

val f
  :  id:'a Type_equal.Id.t
  -> default:'b
  -> for_some:('a -> 'b)
  -> here:Source_code_position.t
  -> ('b, unit) Computation.packed_info Trampoline.t
