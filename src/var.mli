open! Core
open! Import

type 'a t

val create : 'a -> 'a t
val update : 'a t -> f:('a -> 'a) -> unit
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
val value : 'a t -> 'a Value.t
