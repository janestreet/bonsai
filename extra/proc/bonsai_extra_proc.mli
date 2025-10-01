open! Core
open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** [with_inject_fixed_point] allows an injection function produced as the result of a
    computation to be used as the input of that same combination. This "tie-the-knot"
    operation is legal because actions are scheduled independently of computation
    evaluation, allowing us to break what seems like a dependency loop.

    However, it is important that the input injection function isn't the same one that is
    returned by the computation (or even a component of the returned injection function).
    If that happens, an action being triggered will cause an inifinite loop to occur in
    the action scheduler. *)
val with_inject_fixed_point
  :  (('action -> unit Effect.t) Bonsai.t
      -> local_ Bonsai.graph
      -> ('result * ('action -> unit Effect.t)) Bonsai.t)
  -> local_ Bonsai.graph
  -> 'result Bonsai.t

(** [with_self_effect] gives access to an effect which produces the output ['a] within the
    body of [f]. This can be useful if you'd like to install an effect handling function
    on [f] whose logic depends on its current value. *)
val with_self_effect
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> f:
       ('a Bonsai.Computation_status.t Effect.t Bonsai.t
        -> local_ Bonsai.graph
        -> 'a Bonsai.t)
  -> unit
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

(** [pipe] constructs a pipe of [a] and returns a pair containing an injection function
    that enqueues items and an Effect that dequeues them. *)
val pipe
  :  ?sexp_of:('a -> Sexp.t)
  -> local_ Bonsai.graph
  -> (('a -> unit Effect.t) * 'a Effect.t) Bonsai.t

(** As its name implies, [exactly_once] runs the event passed in via [Bonsai.t] exactly
    once. *)
val exactly_once : unit Effect.t Bonsai.t -> local_ Bonsai.graph -> unit Bonsai.t

(** As its name implies, [exactly_once] runs the event passed in via [Bonsai.t] exactly
    once. The return value is stored and returned. [None] is returned while the effect is
    executing. *)
val exactly_once_with_value
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a Effect.t Bonsai.t
  -> local_ Bonsai.graph
  -> 'a option Bonsai.t

(** Extends a Bonsai.t by providing a setter effect that can be used to override the
    returned value. The computation will initially evaluate to the input [Bonsai.t]. Once
    the returned overriding effect is dispatched at least once, the computation will
    evaluate to the override value provided. The effect can be scheduled more than once to
    update the override. Use with [Bonsai.with_model_resetter] in order to revert the
    override. *)
val value_with_override
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> local_ Bonsai.graph
  -> ('a * ('a -> unit Effect.t)) Bonsai.t

(** This function is identical to [Bonsai.state_machine0] except that the [default_model]
    is initially unset, but can be computed or defaulted to a dynamic value.

    This means that before an apply_action occurs, the model can change out from
    underneath the state machine as the default_model value changes. If this is
    undesirable, you may want to [freeze] the default_model first. *)
val state_machine0_dynamic_model
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> unit
  -> local_ Bonsai.graph
  -> ('model * ('action -> unit Effect.t)) Bonsai.t

(** Read the docs for [state_machine0_dynamic_model]. This one has an extra ['input] value
    that can be taken into account when the [apply_action] is invoked. *)
val state_machine1_dynamic_model
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t
        -> 'input
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Bonsai.t
  -> local_ Bonsai.graph
  -> ('model * ('action -> unit Effect.t)) Bonsai.t

(** The analog of [Bonsai.state], but with a dynamic model. Read the docs for
    [state_machine0_dynamic_model].

    When passed a [`Given] model, behaves like [value_with_override]; takes on the initial
    value of the given model and has its value overwritten when the callback is scheduled.

    When passed a [`Computed f] model, takes initial value [f None]. When the callback is
    scheduled with input [i], takes the value [f i]. *)
val state_dynamic_model
  :  ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> unit
  -> local_ Bonsai.graph
  -> ('model * ('model -> unit Effect.t)) Bonsai.t

(** Id_gen builds a component which generates unique identifiers by starting at 0 and
    incrementing by one every time that the effect is called.

    The functor is parameteraized on the size of integer (int vs int63 vs int64), and it's
    also a generative functor, so invoking the functor multiple times times will mint new
    types.

    I explicitly disassociate the input T from the output T because otherwise the benefits
    of the generative functor would be gone.

    The [reset] parameter (default: [`Reset]) can be used to configure the behavior of the
    component when inside of a [with_model_resetter]:
    - [`Reset] fully resets the component, and will start generating ids starting at 0
      again
    - [`Do_nothing] makes the component ignore the reset and keep its state as-is
    - [`Bump] moves the counter up one, fully disassociating it from the previous
      generation of ids. This variant is only useful in [component'] when you'd want no
      previously generated ids to match the current id. *)
module Id_gen (T : Int_intf.S) () : sig
  include Int_intf.S

  val component
    :  ?reset:[ `Reset | `Do_nothing ]
    -> local_ Bonsai.graph
    -> t Bonsai.Effect.t Bonsai.t

  (** [component'] also gives you access to the most recently generated id *)
  val component'
    :  ?reset:[ `Reset | `Do_nothing | `Bump ]
    -> local_ Bonsai.graph
    -> (t Bonsai.Effect.t * t) Bonsai.t
end

(** [mirror] is used to reflect state back and forth between locations. Frequently this
    will be used to back up a components model in a more persistent form of storage, such
    as the URL, or local-storage.

    The gist of this combinator is that if you have two states that you'd like to be
    synchronized, you can feed the "current value" and "set value" functions for both
    states into [mirror] and they'll automatically be kept up to date. Either of these can
    be backed by any kind of structure, but there are some important differences in their
    symmetry.

    When the component is first loaded, [store] has priority, so if the values are
    different, [store] wins, and [interactive] has its value "set". From that point on, if
    either incoming value changes, the opposite setter is called. In the case that both
    [store] and [interactive] change at the same time, the tie is broken in favor of
    [interactive], and [store_set] is called. *)
val mirror
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m Bonsai.t
  -> unit
  -> local_ Bonsai.graph
  -> unit Bonsai.t

(** [mirror'] is like [mirror], but the incoming values have the type ['a option Bonsai.t]
    instead of just ['a Bonsai.t]. When a value is changed and its new value is None, we
    don't propagate it to the other setter. *)
val mirror'
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m option Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m option Bonsai.t
  -> unit
  -> local_ Bonsai.graph
  -> unit Bonsai.t

(** [with_last_modified_time] applies a cutoff to the input value and takes a note of the
    last time the value did not cutoff (in other words, the last time it was changed).

    Whenever the returned computation is activated, the "last time modified" value will be
    reset to the current time. *)
val with_last_modified_time
  :  equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> local_ Bonsai.graph
  -> ('a * Time_ns.t) Bonsai.t

(** [is_stable] indicates whether the input value has changed (according to [equal]) in
    the past specified time span. *)
val is_stable
  :  equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> time_to_stable:Time_ns.Span.t Bonsai.t
  -> local_ Bonsai.graph
  -> bool Bonsai.t

module Stability = Bonsai_extra.Value_stability.Stability

(** [value_stability] determines whether the current value has changed recently, and also
    keeps track of the most recent stable value. *)
val value_stability
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> time_to_stable:Time_ns.Span.t Bonsai.t
  -> local_ Bonsai.graph
  -> 'a Stability.t Bonsai.t

module One_at_a_time : sig
  module Status = Bonsai_extra.One_at_a_time.Status
  module Response = Bonsai_extra.One_at_a_time.Response

  (** Turns the input effect into an effect which ensures that only one instance of it is
      running at a time. If another instance of the effect is already running, then [Busy]
      is returned instead of running the effect. In addition, it also returns a value
      representing whether or not an instance of the effect is in progress.

      CAREFUL: If the effect function raises while it is executing, then the status will
      stay at busy, since the computation is unable to witness that the effect completed. *)
  val effect
    :  ('query -> 'response Effect.t) Bonsai.t
    -> local_ Bonsai.graph
    -> (('query -> 'response Response.t Effect.t) * Status.t) Bonsai.t
end

(** [bonk] returns a function which takes an effect and transforms it into the same
    effect, but it will be enqueued onto the Bonsai action queue instead of running
    immediately. This can be useful in niche situations where you have a batch of effects
    that would otherwise interleave and have bad performance behavior. *)
val bonk : local_ Bonsai.graph -> (unit Effect.t -> unit Effect.t) Bonsai.t

(** [chain_incr_effects input effects] allows you to sequentially schedule effects that
    depend on a common ['a Bonsai.t], while ensuring that no effect will receive a stale
    input. This function short-circuits if the input becomes inactive while actions are
    still being applied.

    This is particularly useful for modeling a set of interacting state machines. The
    outputs of each computation can be collected into a single [Bonsai.t], which is then
    provided to each state machine through an injected action. This util allows model
    recomputations made in the `i`th state machine to be immediately visible to the
    [apply_action] logic of the `i+1`th state machine.

    In contrast, just resolving a value with [let%arr] and scheduling multiple dependent
    effects with `[Effect.Many]` will provide all state machines with the state of the
    world before *any* of them recalculated state. *)
val chain_incr_effects
  :  'a Bonsai.t
  -> local_ Bonsai.graph
  -> (('a -> unit Ui_effect.t) list -> unit Ui_effect.t) Bonsai.t
