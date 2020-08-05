open! Core_kernel
open! Import

type 'a t

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t
include Mapn with type 'a t := 'a t

val named : 'a Type_equal.Id.t -> 'a t
val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
val eval : Environment.t -> 'a t -> 'a Incr.t
val of_incr : 'a Incr.t -> 'a t
