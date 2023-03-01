open! Core
open Bonsai.For_open

(** [with_inject_fixed_point] allows an injection function produced as the
    result of a computation to be used as the input of that same combination.
    This "tie-the-knot" operation is legal because actions are scheduled
    independently of computation evaluation, allowing us to break what seems
    like a dependency loop.

    However, it is important that the input injection function isn't the same
    one that is returned by the computation (or even a component of the returned
    injection function).  If that happens, an action being triggered will cause
    an inifinite loop to occur in the aciton scheduler. *)
val with_inject_fixed_point
  :  (('action -> unit Effect.t) Value.t
      -> ('result * ('action -> unit Effect.t)) Computation.t)
  -> 'result Computation.t

(** [pipe] constructs a pipe of [a] and returns a pair containing an injection
    function that enqueues items and an Effect that dequeues them.  *)
val pipe
  :  (module Bonsai.Model with type t = 'a)
  -> (('a -> unit Effect.t) * 'a Effect.t) Computation.t

(** As its name implies, [exactly_once] runs the event passed in via [Value.t]
    exactly once. *)
val exactly_once : unit Effect.t Value.t -> unit Computation.t

(** As its name implies, [exactly_once] runs the event passed in via [Value.t]
    exactly once.  The return value is stored and returned.  [None] is returned
    while the effect is executing. *)
val exactly_once_with_value
  :  (module Bonsai.Model with type t = 'a)
  -> 'a Effect.t Value.t
  -> 'a option Computation.t

(** Extends a Value.t by providing a setter effect that can be used to override the
    returned value.  The computation will initially evaluate to the input [Value.t].
    Once the returned overriding effect is dispatched at least once, the computation
    will evaluate to the override value provided.  The effect can be scheduled more
    than once to update the override.  Use with [Bonsai.with_model_resetter] in order
    to revert the override. *)
val value_with_override
  :  (module Bonsai.Model with type t = 'a)
  -> 'a Value.t
  -> ('a * ('a -> unit Effect.t)) Computation.t

(** This function is identical to [Bonsai.state_machine0] except that
    the [default_model] is initially unset, but can be computed or defaulted
    to a dynamic value.

    This means that before an apply_action occurs, the model can change out
    from underneath the state machine as the default_model value changes.  If
    this is undesirable, you may want to [freeze] the default_model first. *)
val state_machine0_dynamic_model
  :  (module Bonsai.Model with type t = 'model)
  -> (module Bonsai.Action with type t = 'action)
  -> model:[< `Computed of ('model option -> 'model) Value.t | `Given of 'model Value.t ]
  -> apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'model
        -> 'action
        -> 'model)
  -> ('model * ('action -> unit Effect.t)) Computation.t

(** Read the docs for [state_machine0_dynamic_model]. This one has
    an extra ['input] value that can be taken into account when the
    [apply_action] is invoked. *)
val state_machine1_dynamic_model
  :  (module Bonsai.Model with type t = 'model)
  -> (module Bonsai.Action with type t = 'action)
  -> model:[< `Computed of ('model option -> 'model) Value.t | `Given of 'model Value.t ]
  -> apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'input
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Value.t
  -> ('model * ('action -> unit Effect.t)) Computation.t


(** The analog of [Bonsai.state], but with a dynamic model. Read the docs for
    [state_machine0_dynamic_model] *)
val state_dynamic_model
  :  (module Bonsai.Model with type t = 'model)
  -> model:[< `Computed of ('model option -> 'model) Value.t | `Given of 'model Value.t ]
  -> ('model * ('model -> unit Effect.t)) Computation.t

(** Id_gen builds a component which generates unique identifiers by
    starting at 0 and incrementing by one every time that the effect is called.

    The functor is parameteraized on the size of integer (int vs int63 vs
    int64), and it's also a generative functor, so invoking the functor
    multiple times times will mint new types.

    I explicitly disassociate the input T from the output T because otherwise
    the benefits of the generative functor would be gone. *)
module Id_gen (T : Int_intf.S) () : sig
  include Int_intf.S

  val component : t Bonsai.Effect.t Computation.t
end

(** [mirror] is used to reflect state back and forth between locations.
    Frequently this will be used to back up a components model in a more
    persistent form of storage, such as the URL, or local-storage.

    The gist of this combinator is that if you have two states that you'd
    like to be synchronized, you can feed the "current value" and "set
    value" functions for both states into [mirror] and they'll
    automatically be kept up to date. Either of these can be backed by any
    kind of structure, but there are some important differences in their
    symmetry.

    When the component is first loaded, [store] has priority, so if the
    values are different, [store] wins, and [interactive] has its value
    "set". From that point on, if either incoming value changes, the
    opposite setter is called. In the case that both [store] and
    [interactive] change at the same time, the tie is broken in favor of
    [interactive], and [store_set] is called. *)
val mirror
  :  (module Bonsai.Model with type t = 'm)
  -> store_set:('m -> unit Effect.t) Value.t
  -> store_value:'m Value.t
  -> interactive_set:('m -> unit Effect.t) Value.t
  -> interactive_value:'m Value.t
  -> unit Computation.t

(** [mirror'] is like [mirror], but the incoming values have the type ['a option Value.t]
    instead of just ['a Value.t].  When a value is changed and its new value is None,
    we don't propagate it to the other setter. *)
val mirror'
  :  (module Bonsai.Model with type t = 'm)
  -> store_set:('m -> unit Effect.t) Value.t
  -> store_value:'m option Value.t
  -> interactive_set:('m -> unit Effect.t) Value.t
  -> interactive_value:'m option Value.t
  -> unit Computation.t

(** [with_last_modified_time] applies a cutoff to the input value and takes a
    note of the last time the value did not cutoff (in other words, the last
    time it was changed).

    Whenever the returned computation is activated, the "last time modified" value
    will be reset to the current time. *)
val with_last_modified_time
  :  equal:('a -> 'a -> bool)
  -> 'a Value.t
  -> ('a * Time_ns.t) Computation.t

(** [is_stable] indicates whether the input value has changed (according to
    [equal]) in the past specified time span. *)
val is_stable
  :  equal:('a -> 'a -> bool)
  -> 'a Value.t
  -> time_to_stable:Time_ns.Span.t
  -> bool Computation.t

module Stability : sig
  type 'a t =
    | Stable of 'a
    | Unstable of
        { previously_stable : 'a option
        ; unstable_value : 'a
        }
  [@@deriving sexp, equal]

  val most_recent_stable_value : 'a t -> 'a option
end

(** [value_stability] determines whether the current value has changed
    recently, and also keeps track of the most recent stable value. *)
val value_stability
  :  (module Bonsai.Model with type t = 'a)
  -> 'a Value.t
  -> time_to_stable:Time_ns.Span.t
  -> 'a Stability.t Computation.t

module One_at_a_time : sig
  module Status : sig
    type t =
      | Busy
      | Idle
    [@@deriving sexp]
  end

  module Response : sig
    type 'a t =
      | Result of 'a
      | Busy
    [@@deriving sexp]
  end

  (** Turns the input effect into an effect which ensures that only one instance
      of it is running at a time. If another instance of the effect is already
      running, then [Busy] is returned instead of running the effect. In
      addition, this computation also exposes whether or not an instance of the
      effect is in progress.

      CAREFUL: If the effect function raises while it is executing, then the
      status will stay at busy, since the computation is unable to witness that
      the effect completed. *)
  val effect
    :  ('query -> 'response Effect.t) Value.t
    -> (('query -> 'response Response.t Effect.t) * Status.t) Computation.t
end
