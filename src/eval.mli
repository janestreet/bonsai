open! Core
open! Import

val gather : 'result Computation.t -> 'result Computation.packed_info

val wrap_computation
  :  pack:
       ('result Computation.kind
        -> 'result Computation.packed_info lazy_t
        -> 'result Computation.t)
  -> 'result Computation.kind
  -> 'result Computation.t
