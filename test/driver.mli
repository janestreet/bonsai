open! Core_kernel
open! Import

type ('i, 'm, 'r) t

val create
  :  initial_input:'i
  -> initial_model:'m
  -> ('i, 'm, 'r) Bonsai.t
  -> ('i, 'm, 'r) t

val model : (_, 'm, _) t -> 'm
val set_model : (_, 'm, _) t -> 'm -> unit
val set_input : ('i, _, _) t -> 'i -> unit

(** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
val flush : (_, _, _) t -> unit

val schedule_event : _ t -> Event.t -> unit
val result : (_, _, 'r) t -> 'r
