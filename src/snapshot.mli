(** A snapshot represents the state of a component at an instant in time. *)

open! Core
open! Import

type ('model, 'action, 'result) t

(** Applies the provided action to the model in force at the time that the snapshot was
    created.

    The application of the action is allowed to engage in side-effecting computations,
    including calling the [schedule_event] function to request that further actions be
    enqueued to be applied to the model. *)
val apply_action : ('model, 'action, _) t -> ('model, 'action) Apply_action.t

(** The result of a component is the primary value computed by the component in
    question. At the top level of a UI, this is generally a representation of the view,
    but it's often useful to compute other kinds of results in inner components. *)
val result : (_, _, 'result) t -> 'result Incr.t

(** The lifecycle component of a snapshot contains an optional map of all the activation,
    deactivation, and after_display callbacks. *)
val lifecycle : _ t -> Lifecycle.Collection.t Incr.t option

val lifecycle_or_empty : _ t -> Lifecycle.Collection.t Incr.t

(** Creates a new snapshot. Note that the [apply_action] provided here should apply the
    action in question to the model in force at the time [create] is called. *)
val create
  :  apply_action:('model, 'action) Apply_action.t
  -> lifecycle:Lifecycle.Collection.t Incr.t option
  -> result:'result Incr.t
  -> ('model, 'action, 'result) t

val attribute_positions : Source_code_position.t option -> _ t -> unit
