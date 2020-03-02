open! Core_kernel

(** A Snapshot represents the state of a component at an instant in time. *)

type ('model, 'action, 'result, 'event) t

(** Applies the provided action to the model in force at the time that the snapshot was
    created.

    The application of the action is allowed to engage in side-effecting computations,
    including calling the [schedule_event] function to request that further actions be
    enqueued to be applied to the model. *)
val apply_action
  :  ('model, 'action, _, 'event) t
  -> schedule_event:('event -> unit)
  -> 'action
  -> 'model

(** The result of a component is the primary value computed by the component in
    question. At the toplevel of a UI, this is generally a representation of the view,
    but it's often useful to compute other kinds of results in inner components. *)
val result : (_, _, 'result, _) t -> 'result

(** Creates a new snapshot. Note that the [apply_action] provided here should apply the
    action in question to the model as it exists at the time the snapshot is created
    for. *)
val create
  :  apply_action:(schedule_event:('event -> unit) -> 'action -> 'model)
  -> result:'result
  -> ('model, 'action, 'result, 'event) t
