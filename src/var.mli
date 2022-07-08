open! Core
open! Import

(* $MDX part-begin=var_module_signature *)
type 'a t

(** Creates a var with an initial value. *)
val create : 'a -> 'a t

(** Runs a function over the current value and updates it to the result. *)
val update : 'a t -> f:('a -> 'a) -> unit

(** Change the current value. *)
val set : 'a t -> 'a -> unit

(** Retrieve the current value. *)
val get : 'a t -> 'a

(** Get a value that tracks the current value, for use in a computation. *)
val value : 'a t -> 'a Value.t
(* $MDX part-end *)
