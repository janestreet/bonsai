open! Core
open! Import

val compute
  :  ?here:Stdlib.Lexing.position
  -> 'a Value.t
  -> f:('a Incr.t -> 'b Incr.t)
  -> 'b Computation.t

val with_clock
  :  ?here:Stdlib.Lexing.position
  -> (Time_source.t -> 'a Incr.t)
  -> 'a Computation.t

val to_value : ?here:Stdlib.Lexing.position -> 'a Incr.t -> 'a Value.t
