open! Core
open! Import

val f
  :  gather:'a Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> inner:'a Computation.t
  -> reset_id:unit Effect.t Type_equal.Id.t
  -> here:Source_code_position.t
  -> ('a, unit) Computation.packed_info Trampoline.t
