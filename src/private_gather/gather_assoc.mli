open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val unzip3_mapi'
  :  ('a, 'b, 'c) Map.t Incr.t
  -> contains_lifecycle:May_contain.resolved May_contain.Single.t
  -> contains_input:May_contain.resolved May_contain.Single.t
  -> comparator:('a, 'c) Comparator.Module.t
  -> f:(key:'a -> data:'b Incr.t -> 'd Incr.t * 'e Incr.t * 'f Incr.t)
  -> ('a, 'd, 'c) Map.t Incr.t * ('a, 'e, 'c) Map.t Incr.t * ('a, 'f, 'c) Map.t Incr.t

val f
  :  gather:'r Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> map:('k, 'v, 'cmp) Map_intf.Map.t Value.t
  -> key_comparator:('k, 'cmp) Comparator.Module.t
  -> key_id:'k Type_equal.Id.t
  -> cmp_id:'cmp Type_equal.Id.t
  -> data_id:'v Type_equal.Id.t
  -> by:'r Computation.t
  -> here:Source_code_position.t
  -> (('k, 'r, 'cmp) Map_intf.Map.t, unit) Computation.packed_info Trampoline.t
