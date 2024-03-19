open! Core
open! Import

val gather : 'result Computation.t -> ('result, unit) Computation.packed_info
