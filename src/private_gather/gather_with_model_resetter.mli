open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'a Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> inner:'a Computation.t
  -> reset_id:unit Effect.t Type_equal.Id.t
  -> here:Source_code_position.t
  -> ('a, unit) Computation.packed_info Trampoline.t
