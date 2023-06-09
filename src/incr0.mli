open! Core
open! Import

val compute : 'a Value.t -> f:('a Incr.t -> 'b Incr.t) -> 'b Computation.t
val with_clock : (Time_source.t -> 'a Incr.t) -> 'a Computation.t
val to_value : 'a Incr.t -> 'a Value.t
