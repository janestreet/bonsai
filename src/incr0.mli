open! Core
open! Import

val compute
  :  here:[%call_pos]
  -> 'a Value.t
  -> f:('a Incr.t -> 'b Incr.t)
  -> 'b Computation.t

val with_clock : here:[%call_pos] -> (Time_source.t -> 'a Incr.t) -> 'a Computation.t
val to_value : here:[%call_pos] -> 'a Incr.t -> 'a Value.t
