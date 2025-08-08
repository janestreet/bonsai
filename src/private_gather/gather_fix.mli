open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val define
  :  gather:'a Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> fix_id:'a Fix_id.t
  -> initial_input:'b Value.t
  -> input_id:'b Type_equal.Id.t
  -> result:'a Computation.t
  -> here:Source_code_position.t
  -> ('a, unit) Computation.packed_info Trampoline.t

val recurse
  :  recursive_scopes:Computation.Recursive_scopes.t
  -> input:'a Value.t
  -> input_id:'a Type_equal.Id.t
  -> fix_id:'b Fix_id.t
  -> here:Source_code_position.t
  -> ('b, unit) Computation.packed_info Trampoline.t
