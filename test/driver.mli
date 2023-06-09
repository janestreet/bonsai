open! Core
open! Import

type ('i, 'r) t

val create
  :  ?optimize:bool
  -> clock:Bonsai.Time_source.t
  -> initial_input:'i
  -> ('i, 'r) Bonsai.Arrow_deprecated.t
  -> ('i, 'r) t

val set_input : ('i, _) t -> 'i -> unit

(** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
val flush : _ t -> unit

val schedule_event : _ t -> unit Ui_effect.t -> unit
val result : (_, 'r) t -> 'r
val last_view : _ t -> string
val store_view : _ t -> string -> unit
val trigger_lifecycles : _ t -> unit
val has_after_display_events : _ t -> bool

val sexp_of_model : _ t -> Sexp.t
val input : ('i, _) t -> 'i
val result_incr : (_, 'r) t -> 'r Incr.t
val lifecycle_incr : _ t -> Incr.Packed.t
val action_input_incr : _ t -> Incr.Packed.t
val clock : (_, _) t -> Bonsai.Time_source.t
val invalidate_observers : _ t -> unit
val reset_model_to_default : _ t -> unit
