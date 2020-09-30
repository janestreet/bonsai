open! Core_kernel
open! Import

type ('i, 'r) t

val create
  :  ?initial_model_sexp:Sexp.t
  -> initial_input:'i
  -> ('i, 'r) Bonsai.Arrow.t
  -> ('i, 'r) t

val set_input : ('i, _) t -> 'i -> unit

(** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
val flush : _ t -> unit

val schedule_event : _ t -> Event.t -> unit
val result : (_, 'r) t -> 'r
val last_view : _ t -> string
val store_view : _ t -> string -> unit

val sexp_of_model : _ t -> Sexp.t
val input : ('i, _) t -> 'i
