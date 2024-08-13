open! Core
open! Import

val gather
  :  here:Source_code_position.t
  -> info_from:(_, _, _, 'from_result, unit) Computation.info
  -> info_into:(_, _, _, 'into_result, unit) Computation.info
  -> via:'from_result Type_equal.Id.t
  -> ('into_result, unit) Computation.packed_info

(** [eval.ml]'s [gather] function depends on [eval_sub.ml], so we can't
    call [Eval.gather] from here, so it needs to be pased in while packed
    up in this polymorphic type. *)
type generic_gather =
  { f :
      'a.
      recursive_scopes:Computation.Recursive_scopes.t
      -> time_source:Time_source.t
      -> 'a Computation.t
      -> ('a, unit) Computation.packed_info Trampoline.t
  }

(** [chain] is an optimized gather implementation for Sub nodes that contains more Sub
    nodes down the 'into' side. *)
val chain
  :  'a Computation.t
  -> gather:generic_gather
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> ('a, unit) Computation.packed_info Trampoline.t
