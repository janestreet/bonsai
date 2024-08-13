open! Core
open! Import

val f : value:'a Value.t -> ('a, unit) Computation.packed_info Trampoline.t
