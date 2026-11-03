open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'result Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> bound:'bound Value.t
  -> comparator:('bound, 'cmp) Comparator.Module.t
  -> bound_id:'bound Var_id.t
  -> cmp_id:'cmp Var_id.t
  -> f:('bound -> 'result Computation.t)
  -> here:Source_code_position.t
  -> ('result, unit) Computation.packed_info Trampoline.t
