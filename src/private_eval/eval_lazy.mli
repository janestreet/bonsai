open! Core
open! Import

val f
  :  gather:'a Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> lazy_computation:'a Computation.t lazy_t
  -> ('a, unit) Computation.packed_info Trampoline.t
