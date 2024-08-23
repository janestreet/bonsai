open! Core
open! Import

(* $MDX part-begin=var_module_signature *)
type 'a t

(** Creates a var with an initial value. *)
val create : 'a -> 'a t

(** Runs a function over the current value and updates it to the result. *)
val update : 'a t -> f:('a -> 'a) -> unit

(** Change the current value. *)
val set : here:[%call_pos] -> 'a t -> 'a -> unit

(** Retrieve the current value. *)
val get : 'a t -> 'a

(** Get a value that tracks the current value, for use in a computation. *)
val value : here:[%call_pos] -> 'a t -> 'a Value.t

(* $MDX part-end *)

(** Retrieves the underlying ['a t] Ui_incr.t var. *)
val incr_var : 'a t -> 'a Ui_incr.Var.t
