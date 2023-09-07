open! Core
open! Import

val baseline
  :  here:Source_code_position.t option
  -> info_from:(_, _, _, 'from_result) Computation.info
  -> info_into:(_, _, _, 'into_result) Computation.info
  -> via:'from_result Type_equal.Id.t
  -> 'into_result Computation.packed_info

val from_stateless
  :  here:Source_code_position.t option
  -> info_from:(unit, Nothing.t Action.leaf, _, 'from_result) Computation.info
  -> info_into:(_, _, _, 'into_result) Computation.info
  -> via:'from_result Type_equal.Id.t
  -> 'into_result Computation.packed_info

val into_stateless
  :  here:Source_code_position.t option
  -> info_from:(_, _, _, 'from_result) Computation.info
  -> info_into:(unit, Nothing.t Action.leaf, _, 'into_result) Computation.info
  -> via:'from_result Type_equal.Id.t
  -> 'into_result Computation.packed_info
