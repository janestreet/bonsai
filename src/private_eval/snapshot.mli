(** A snapshot represents the state of a component at an instant in time. *)

open! Core
open! Import

type ('model, 'input, 'result) t

(** The input in a snapshot contains any incrementally computed data required by the
    computation's apply-action function. *)
val input : (_, 'input, _) t -> 'input Input.t

(** The result of a component is the primary value computed by the component in question.
    At the top level of a UI, this is generally a representation of the view, but it's
    often useful to compute other kinds of results in inner components. *)
val result : (_, _, 'result) t -> 'result Incr.t

(** The lifecycle component of a snapshot contains an optional map of all the activation,
    deactivation, and after_display callbacks. *)
val lifecycle : _ t -> Lifecycle.Collection.t Incr.t option

val lifecycle_or_empty
  :  here:Source_code_position.t
  -> _ t
  -> Lifecycle.Collection.t Incr.t

(** Creates a new snapshot. Note that the [apply_action] provided here should apply the
    action in question to the model in force at the time [create] is called. *)
val create
  :  here:Source_code_position.t
  -> input:'input Input.t
  -> lifecycle:Lifecycle.Collection.t Incr.t option
  -> result:'result Incr.t
  -> ('model, 'input, 'result) t

val attribute_positions : Source_code_position.t -> _ t -> unit
