open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'r Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> match_:int Value.t
  -> arms:(int, 'r Computation.t, Base.Int.comparator_witness) Map_intf.Map.t
  -> here:Source_code_position.t
  -> ('r, unit) Computation.packed_info Trampoline.t
