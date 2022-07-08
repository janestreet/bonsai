open! Core
open Bonsai.For_open

(* A bool-state which starts at [default_model] and flips whenever the
   returned effect is scheduled. *)
val toggle : default_model:bool -> (bool * unit Effect.t) Computation.t

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

(** [yoink] is a function that takes a bonsai value and produces a
    computation producing an effect which fetches the current value out of the
    input.  This can be useful inside of [let%bind.Effect] chains, where a
    value that you've closed over is stale and you want to witness a value
    after it's been changed by a previous effect. *)
val yoink : 'a Value.t -> 'a Effect.t Computation.t

(** [scope_model] allows you to have a different model for the provided
    computation, keyed by some other value.

    Suppose for example, that you had a form for editing details about a
    person.  This form should have different state for each person.  You could
    use scope_model, where the [~on] parameter is set to a user-id, and now when
    that value changes, the model for the other computation is set to the model
    for that particular user. *)
val scope_model
  :  ('a, _) Bonsai.comparator
  -> on:'a Value.t
  -> 'b Computation.t
  -> 'b Computation.t

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
