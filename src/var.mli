open! Core_kernel
open! Import

type 'a t

val create : 'a -> 'a t
val update : 'a t -> f:('a -> 'a) -> unit
val set : 'a t -> 'a -> unit
val value : 'a t -> 'a Value.t
