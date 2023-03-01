open! Core
open! Import

type 'input t

val dynamic : 'input Incr.t -> 'input t
val static : unit t
val to_incremental : 'input t -> 'input Incr.t
val merge : 'input1 t -> 'input2 t -> ('input1 * 'input2) t
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter_incremental : _ t -> f:(Incremental.Packed.t -> unit) -> unit
