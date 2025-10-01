open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'r Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> id:'a Type_equal.Id.t
  -> value:'a Value.t
  -> inner:'r Computation.t
  -> ('r, unit) Computation.packed_info Trampoline.t
