open! Core
open! Import

val gather
  :  recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> 'result Computation.t
  -> ('result, unit) Computation.packed_info
