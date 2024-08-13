open! Core
open! Import

val f
  :  gather:'a Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> inner:'a Computation.t
  -> here:Source_code_position.t
  -> free_vars:Type_id_set.t
  -> ('a, unit) Computation.packed_info Trampoline.t
