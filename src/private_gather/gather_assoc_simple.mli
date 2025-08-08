open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  map:('key, 'data, 'cmp) Base.Map.t Value.t
  -> by:(Path.t -> 'key -> 'data -> 'result)
  -> may_contain:May_contain.Resolved.t
  -> here:Source_code_position.t
  -> (('key, 'result, 'cmp) Base.Map.t, unit) Computation.packed_info Trampoline.t
