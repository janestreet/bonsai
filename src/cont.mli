open! Core
open! Import

module type Enum = Module_types.Enum

(** The primary type in the Bonsai library, you can think of a value with type ['a t] as
    "a 'a that changes over time". The two main ways that you get a value with this type
    are:
    1. by creating a state machine
       - the current value of the state machine is returned as a ['state Bonsai.t]
       - the "send the state machine an action" function is also inside of a [Bonsai.t]
    2. by mapping on existing [Bonsai.t]'s to derive a new computed [Bonsai.t] *)
type 'a t

(** [Bonsai.graph] is a required parameter to all Bonsai functions which do more than pure
    computation. The value is always [local_] because Bonsai applications have two phases:
    1. The graph building phase. This is the phase where you have access to a
       [local_ Bonsai.graph]
    2. Runtime. The application has started and modifying the graph is no longer
       permitted. *)
type graph

(** [Bonsai.t] is an applicative, which means that you can [let%map] on things. In
    practice, you should prefer [let%arr] which is conceptually equivalent to [let%map],
    but has some performance benefits. *)
include Applicative.S with type 'a t := 'a t

include Mapn with type 'a t := 'a t

module Autopack : sig
  type 'a bonsai := 'a t

  (** Several Bonsai combinators take functions as a parameter where the user-provided
      function returns a [Bonsai.t]. If the user has _multiple_ [Bonsai.t] that they'd
      like to return, then they'd need to pack them up, e.g. going from
      [int Bonsai.t * string Bonsai.t] to a [(int * string) Bonsai.t] to return it. Then,
      if the function returns the single [Bonsai.t], you may need to unpack the values
      again.

      To reduce boilerplate, these functions also have a version that automatically packs
      and unpacks tuples of Bonsai.t. For example, instead of writing

      {[
        let results, reset_effect =
          Bonsai.with_model_resetter graph ~f:(fun ~reset (local_ graph) ->
            let a, b = Bonsai.state None graph in
            let c, d = Bonsai.state None graph in
            let%arr a and b and c and d in
            a, b, c, d)
        in
        let%sub (a, b, c, d) = results in
        ...
      ]}

      you can use version which takes an [Autopack.t]:

      {[
        let (a, b, c, d), reset_effect =
          Bonsai.with_model_resetter_n graph ~n:Four ~f:(fun ~reset (local_ graph) ->
            let a, b = Bonsai.state None graph in
            let c, d = Bonsai.state None graph in
            a, b, c, d)
        in
        ...
      ]} *)
  type ('packed, 'unpacked) t =
    | One : ('a, 'a bonsai) t
    | Two : ('a * 'b, 'a bonsai * 'b bonsai) t
    | Three : ('a * 'b * 'c, 'a bonsai * 'b bonsai * 'c bonsai) t
    | Four : ('a * 'b * 'c * 'd, 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai) t
    | Five :
        ( 'a * 'b * 'c * 'd * 'e
          , 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai * 'e bonsai )
          t
    | Six :
        ( 'a * 'b * 'c * 'd * 'e * 'f
          , 'a bonsai * 'b bonsai * 'c bonsai * 'd bonsai * 'e bonsai * 'f bonsai )
          t
    | Seven :
        ( 'a * 'b * 'c * 'd * 'e * 'f * 'g
          , 'a bonsai
            * 'b bonsai
            * 'c bonsai
            * 'd bonsai
            * 'e bonsai
            * 'f bonsai
            * 'g bonsai )
          t
end

(** [return] produces a [Bonsai.t] whose inner value is constant. *)
val return : here:[%call_pos] -> 'a -> 'a t

(** [return_lazy] produces a [Bonsai.t] whose inner value is only computed when it becomes
    necessary. *)
val return_lazy : here:[%call_pos] -> 'a Lazy.t -> 'a t

(** [map], [map2], and [both] are ways to build a new [Bonsai.t] which is dependent on the
    values of other [Bonsai.t]. As noted above, you should prefer to use [let%arr] than
    these functions, because they come with some performance benefits. *)
val map : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t

val map2 : here:[%call_pos] -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val both : here:[%call_pos] -> 'a t -> 'b t -> ('a * 'b) t

(** The Bonsai runtime will recompute a node if any of its dependencies change. But
    sometimes, you may want to consider two contained values to be "close enough" and cut
    off recomputation. You can do that by passing a custom equality function to
    [Bonsai.cutoff]. *)
val cutoff : here:[%call_pos] -> 'a t -> equal:('a -> 'a -> bool) -> 'a t

(** [transpose_opt] flips the order of a [Bonsai.t] and an [option]. This is useful for
    optional args that take [Bonsai.t]s. Note: the inverse operation is not possible. *)
val transpose_opt : here:[%call_pos] -> 'a t option -> 'a option t

(** [Bonsai.state] allocates a stateful Bonsai.t node in the graph. It returns both the
    [Bonsai.t] containing the current state, as well as a [Bonsai.t] containing a function
    for overwriting the state.

    You must provide a "starting" value for the state.

    [?reset] is called when the reset effect of a grandparent [Bonsai.with_model_resetter]
    is called, and allows you to decide what to do when the model is reset. It defaults to
    a function that ignores the current model and returns the "starting" value.

    [?sexp_of_model] is only used to give the debugger more information.

    [?equal] (default [phys_equal]) is used by some combinators to reduce memory usage.
    (E.g. [assoc] and [match%sub] may determine that they can store a reference to only
    the default model rather than the current model.) *)
val state
  :  here:[%call_pos]
  -> ?reset:('model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> 'model
  -> local_ graph
  -> 'model t * ('model -> unit Effect.t) t

(** [state_opt] is just like [state] except that the model is optional. The model starts
    out as [None] unless you provide a value to the [default_model] optional parameter. *)
val state_opt
  :  here:[%call_pos]
  -> ?reset:('model option -> 'model option)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> ?default_model:'model
  -> local_ graph
  -> 'model option t * ('model option -> unit Effect.t) t

(** Similar to [state], but the `set` function takes a function that calculates the new
    state from the previous state. *)
val state'
  :  here:[%call_pos]
  -> ?reset:('model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> 'model
  -> local_ graph
  -> 'model t * (('model -> 'model) -> unit Effect.t) t

(** [Bonsai.toggle] is a small helper function for building a [bool] state that toggles
    back and forth between [true] and [false] whenever the [unit Effect.t] is scheduled. *)
val toggle
  :  here:[%call_pos]
  -> default_model:bool
  -> local_ graph
  -> bool t * unit Effect.t t

module Toggle : sig
  (** For the more advanced toggle function [Bonsai.toggle'] we return the state, the
      toggling function, and a function to set the state directly. *)
  type nonrec t =
    { state : bool t
    ; set_state : (bool -> unit Effect.t) t
    ; toggle : unit Effect.t t
    }
end

(** Just like [toggle] except that you also have an [Effect.t] for directly setting the
    state in addition to toggling it back and forth. *)
val toggle' : here:[%call_pos] -> default_model:bool -> local_ graph -> Toggle.t

module Apply_action_context : sig
  (** A value with the type [('action, 'response) Apply_action_context.t] is provided to
      all state-machine's [apply_action] functions. It can be used to do a variety of
      things that are only legal inside of [apply_action]:
      1. Access the application time source directly. This is most likely useful to read
         the current time or sleep for some time span
      2. "inject" a value corresponding to the state-machine's action type into an effect
         that can be scheduled.
      3. Directly schedule effects. *)

  type ('action, 'response) t = ('action, 'response) Apply_action_context.t

  val inject : ('action, 'response) t -> 'action -> 'response Effect.t
  val schedule_event : _ t -> unit Effect.t -> unit
  val time_source : _ t -> Time_source.t
end

type ('model, 'action, 'response) resetter :=
  ('action, 'response) Apply_action_context.t -> 'model -> 'model

(** [Bonsai.state_machine] allows you to build a state machine whose state is initialized
    to whatever you pass to [default_model], and the state machine transitions states
    using the [apply_action] function. The current state and a function for injecting an
    action into a schedulable effect are returned.

    [?sexp_of_action] is used to provide more information to debugging tools.

    [?reset], [?sexp_of_model], and [?equal] do the same things that they do for
    [Bonsai.state], go read those docs for more.

    Previously called [state_machine0] *)
val state_machine
  :  here:[%call_pos]
  -> ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:(('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model)
  -> local_ graph
  -> 'model t * ('action -> unit Effect.t) t

module Computation_status : sig
  (** A [Computation_status.t] is used by [state_machine_with_input] and
      [actor_with_input] in order to signal to their action-application functions that the
      state machine is inactive because an inactive state machine doesn't have access to
      its input. *)
  type 'input t =
    | Active of 'input
    | Inactive
  [@@deriving sexp_of]
end

(** A [state_machine_with_input] is identical to a [state_machine] except that you can
    pass an arbitrary ['a Bonsai.t] dependency and have access to a
    ['a Computation_status.t] inside of the apply_action function.

    In the event that the state-machine is inactive, the computation status will be
    [Inactive]

    Previously called [state_machine1] *)
val state_machine_with_input
  :  here:[%call_pos]
  -> ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> 'input t
  -> local_ graph
  -> 'model t * ('action -> unit Effect.t) t

(** [Bonsai.actor] is similar to [Bonsai.state_machine], but its [recv] function is
    responsible for not only transitioning the state of the state machine, but also for
    responding with a "return value" to whoever sent the message to the actor. *)
val actor
  :  here:[%call_pos]
  -> ?reset:('model, 'action, 'return) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> recv:
       (('action, 'return) Apply_action_context.t
        -> 'model
        -> 'action
        -> 'model * 'return)
  -> local_ graph
  -> 'model t * ('action -> 'return Effect.t) t

(** [actor_with_input] is just like [actor] but it can witness the current value of a
    [Bonsai.t] inside its [recv] function just like [state_machine_with_input] *)
val actor_with_input
  :  here:[%call_pos]
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?reset:('model, 'action, 'return) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> recv:
       (('action, 'return) Apply_action_context.t
        -> 'input Computation_status.t
        -> 'model
        -> 'action
        -> 'model * 'return)
  -> 'input t
  -> local_ graph
  -> 'model t * ('action -> 'return Effect.t) t

module Actor (Action : T1) : sig
  (** Unlike the traditional [actor] and [actor_with_input] functions, the [Actor] functor
      allows the type of the value returned inside of the effect to change based on which
      action is being handled.

      Example:
      {[
        module Action = struct
          type 'a t =
            | Increment : int t
            | Get_string : string -> string t
        end

        module My_actor = Actor (Action)

        let model, inject =
          My_actor.create
            ~default_model:0
            ~recv:
              { f =
                  (fun _ctx model -> function
                    | Action.Increment -> model + 1, model
                    | Action.Get_string s -> model, sprintf "Got %s at count %d" s model)
              }
            graph
        ;;
      ]} *)

  type sexp_of_action = { f : 'a. 'a Action.t -> Sexp.t }
  type inject = { f : 'a. 'a Action.t -> 'a Effect.t }

  type get_apply_action_context =
    { get : 'a. unit -> ('a Action.t, 'a) Apply_action_context.t }

  type ('model, 'input) recv_with_input =
    { f :
        'a.
        get_apply_action_context
        -> 'input Computation_status.t
        -> 'model
        -> 'a Action.t
        -> 'model * 'a
    }

  (** [create_with_input] creates a stateful component that depends on external input. The
      [recv] function handles actions and returns both an updated model and a typed
      response. *)
  val create_with_input
    :  here:[%call_pos]
    -> ?sexp_of_action:sexp_of_action
    -> ?reset:(get_apply_action_context -> 'model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:('model, 'input) recv_with_input
    -> 'input t
    -> local_ graph
    -> 'model t * inject t

  type 'model recv =
    { f : 'a. get_apply_action_context -> 'model -> 'a Action.t -> 'model * 'a }

  (** [create] creates a stateful component without external input dependencies. The
      [recv] function handles actions and returns both an updated model and a typed
      response. *)
  val create
    :  here:[%call_pos]
    -> ?sexp_of_action:sexp_of_action
    -> ?reset:(get_apply_action_context -> 'model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:'model recv
    -> local_ graph
    -> 'model t * inject t
end

(** [freeze] takes a ['a Bonsai.t] and returns a new ['a Bonsai.t] whose value is frozen
    to the first value that it witnesses from its input. *)
val freeze
  :  here:[%call_pos]
  -> ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a t
  -> local_ graph
  -> 'a t

(** A fixed-point combinator for writing recursive Bonsai components. *)
val fix
  :  here:[%call_pos]
  -> 'input t
  -> f:
       (recurse:('input t -> local_ graph -> 'result t)
        -> 'input t
        -> local_ graph
        -> 'result t)
  -> local_ graph
  -> 'result t

(** Just like [fix] but specialized for 2-arity functions. *)
val fix2
  :  here:[%call_pos]
  -> 'a t
  -> 'b t
  -> f:
       (recurse:('a t -> 'b t -> local_ graph -> 'result t)
        -> 'a t
        -> 'b t
        -> local_ graph
        -> 'result t)
  -> local_ graph
  -> 'result t

(** [scope_model] is used to associate the state of some component graph with some other
    value. *)
val scope_model
  :  here:[%call_pos]
  -> ('a, _) Comparator.Module.t
  -> on:'a t
  -> for_:(local_ graph -> 'b t)
  -> local_ graph
  -> 'b t

(** Exactly like [scope_model] but will automatically pack and unpack any number of
    [Bonsai.t] produced inside the [for_] closure. See documentation for [Autopack.t] for
    more info. *)
val scope_model_n
  :  here:[%call_pos]
  -> ('a, _) Comparator.Module.t
  -> n:(_, 'b) Autopack.t
  -> on:'a t
  -> for_:(local_ graph -> 'b)
  -> local_ graph
  -> 'b

(** [Bonsai.most_recent_some] can be used to find and store a value that has some
    interesting property by transforming a ['a Bonsai.t] into a ['b option Bonsai.t] via a
    [f:('a -> 'b option)] function. The output of this function is a ['b option Bonsai.t]
    that contains the most recent output of [f] that returned a [Some]. *)
val most_recent_some
  :  here:[%call_pos]
  -> ?sexp_of_model:('b -> Sexp.t)
  -> equal:('b -> 'b -> bool)
  -> 'a t
  -> f:('a -> 'b option)
  -> local_ graph
  -> 'b option t

(** The output of [most_recent_value_satisfying] is a ['a option Bonsai.t] that contains
    the most recent input value for for which [f] returned [true ]. *)
val most_recent_value_satisfying
  :  here:[%call_pos]
  -> ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a t
  -> condition:('a -> bool)
  -> local_ graph
  -> 'a option t

(** [previous_value] returns a [Bonsai.t] which contains the value that its input held on
    the previous frame. *)
val previous_value
  :  here:[%call_pos]
  -> ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a t
  -> local_ graph
  -> 'a option t

(** [wrap] wraps a Computation (built using [f]) and provides a model and injection
    function that the wrapped component can use. Especially of note is that the
    [apply_action] for this outer-model has access to the result value of the Computation
    being wrapped. ['result] is wrapped in a [Computation_status.t] so that you are able
    to reset the underlying model even if the computation is inactive *)
val wrap
  :  here:[%call_pos]
  -> ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t
        -> 'result Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> f:('model t -> ('action -> unit Effect.t) t -> local_ graph -> 'result t)
  -> local_ graph
  -> 'result t

(** Exactly like [wrap] but will automatically pack and unpack any number of [Bonsai.t]
    produced inside the [f] closure. See documentation for [Autopack.t] for more info. *)
val wrap_n
  :  here:[%call_pos]
  -> ?reset:('model, 'action, unit) resetter
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t
        -> 'packed Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> f:('model t -> ('action -> unit Effect.t) t -> local_ graph -> 'unpacked)
  -> n:('packed, 'unpacked) Autopack.t
  -> local_ graph
  -> 'unpacked

(** [enum] is used for matching on a value and providing different behaviors on different
    values. The type of the value must be enumerable (there must be a finite number of
    possible values), and it must be comparable and sexpable.

    The rest of the parameters are named like you might expect from pattern-matching
    syntax, with [match_] taking the value to match on, and [with_] taking a function that
    choose which behavior to use. *)
val enum
  :  here:[%call_pos]
  -> (module Enum with type t = 'k)
  -> match_:'k t
  -> with_:('k -> local_ graph -> 'a t)
  -> local_ graph
  -> 'a t

(** [with_inverted_lifecycle_ordering] will run [compute_dep] and pass its result to [f],
    but any lifecycles registered in [f] will run before those registered in
    [compute_dep]. *)
val with_inverted_lifecycle_ordering
  :  here:[%call_pos]
  -> compute_dep:(local_ graph -> 'dep t)
  -> f:('dep t -> local_ graph -> 'a t)
  -> local_ graph
  -> 'a t

(** All stateful components allocated inside of [with_model_resetter]'s [f] callback will
    have their [reset] functions invoked whenever the [unit Effect.t] returned by
    [with_model_resetter] is scheduled. *)
val with_model_resetter
  :  here:[%call_pos]
  -> f:(local_ graph -> 'a t)
  -> local_ graph
  -> 'a t * unit Effect.t t

(** Exactly like [with_model_resetter] but will automatically pack and unpack any number
    of [Bonsai.t] produced inside the [f] closure. See documentation for [Autopack.t] for
    more info. *)
val with_model_resetter_n
  :  here:[%call_pos]
  -> f:(local_ graph -> 'a)
  -> n:(_, 'a) Autopack.t
  -> local_ graph
  -> 'a * unit Effect.t t

(** The same as [with_model_resetter], but the closure has access to the [reset] function
    this time. *)
val with_model_resetter'
  :  here:[%call_pos]
  -> f:(reset:unit Effect.t t -> local_ graph -> 'a t)
  -> local_ graph
  -> 'a t

(** Exactly like [with_model_resetter'] but will automatically pack and unpack any number
    of [Bonsai.t] produced inside the [f] closure. See documentation for [Autopack.t] for
    more info. *)
val with_model_resetter_n'
  :  here:[%call_pos]
  -> f:(reset:unit Effect.t t -> local_ graph -> 'a)
  -> n:(_, 'a) Autopack.t
  -> local_ graph
  -> 'a

(** [peek] maps a [Bonsai.t] to an [Effect.t] with the same underlying value. This allows
    you to inspect the ['a] value from inside of a [let%bind.Effect] chain which might
    have been changed by previous effects. It is analogous to [peek] on other abstract
    data types, including [Deferred.t]s and [Mvar.t]s, but more constrained in that you
    still can only read from within an effect bind.

    The ['a Computation_state.t] returned by the effect means that if the value was
    inactive at the time it is peeked, then the effect will be unable to retrieve it. *)
val peek : here:[%call_pos] -> 'a t -> local_ graph -> 'a Computation_status.t Effect.t t

(** Various clock and timing functions *)
module Clock : sig
  (** Similar to [now], but instead of updating every frame, it will only update every
      [tick_every] time span. *)
  val approx_now
    :  here:[%call_pos]
    -> tick_every:Time_ns.Span.t
    -> local_ graph
    -> Time_ns.t t

  module Before_or_after : sig
    type t = Ui_incr.Before_or_after.t =
      | Before
      | After
    [@@deriving sexp, equal]
  end

  (** [Bonsai.Clock.at] can tell you if a given [Time_ns.t Bonsai.t] is before or after
      the current wall clock time. *)
  val at : here:[%call_pos] -> Time_ns.t t -> local_ graph -> Before_or_after.t t

  (** {v
 An event passed to [every] is scheduled on an interval determined by
      the time-span argument.

      [when_to_start_next_effect] has the following behavior
      | `Wait_period_after_previous_effect_starts_blocking -> If the previous effect takes longer than [period], we wait until it finishes before starting the next effect.
      | `Wait_period_after_previous_effect_finishes_blocking -> The effect will always be executed [period] after the previous effect finishes.
      | `Every_multiple_of_period_non_blocking -> Executes the effect at a regular interval.
      | `Every_multiple_of_period_blocking -> Same as `Every_multiple_of_second, but skips a beat if the previous effect is still running.
      v} *)
  val every
    :  here:[%call_pos]
    -> when_to_start_next_effect:
         [< `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
    -> ?trigger_on_activate:bool
    -> Time_ns.Span.t t
    -> unit Effect.t t
    -> local_ graph
    -> unit

  (** Produces an effect that can be used to query the current time. *)
  val get_current_time : here:[%call_pos] -> local_ graph -> Time_ns.t Effect.t t

  (** Produces an effect that can be used to sleep for some amount of time. *)
  val sleep : here:[%call_pos] -> local_ graph -> (Time_ns.Span.t -> unit Effect.t) t

  (** Produces an effect that can be used to sleep until some timestamp. *)
  val until : here:[%call_pos] -> local_ graph -> (Time_ns.t -> unit Effect.t) t

  module Expert : sig
    (** Returns a [Bonsai.t] that contains the current time. Using this value can lead to
        inefficient programs that are continuously recomputing because the current time
        changes on every frame. Consider using [approx_now] instead, if possible. *)
    val now : here:[%call_pos] -> local_ graph -> Time_ns.t t
  end
end

module Edge : sig
  (** [Bonsai.Edge.on_change] is used to schedule an effect whenever some [Bonsai.t]
      changes. The [equal] function is used to determine if a value is the same or not.
      The callback is _always_ invoked the first time that this component is becomes
      active. The callback is invoked at most once per frame and will be called with the
      latest value. [?sexp_of_model] is only used for debugging.

      [`Before_display] can be used to react to changes in the same frame that they
      happen, eliminating certain frame gaps. However, the callback is still only invoked
      at most once per frame; if two different [before_display]s cause [t] to change, the
      callback will only be invoked on one of the new values. *)
  val on_change
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> trigger:[ `Before_display | `After_display ]
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a -> unit Effect.t) t
    -> local_ graph
    -> unit

  (** Just like [on_change] except that the callback function also has access to the
      previous value. The previous value will be [None] when [callback] is first invoked
      because the callback is _always_ invoked the first time that this component is
      becomes active. *)
  val on_change'
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> trigger:[ `Before_display | `After_display ]
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a option -> 'a -> unit Effect.t) t
    -> local_ graph
    -> unit

  (** [Bonsai.Edge.lifecycle] is used to add effects to Bonsai's lifecycle events. Using
      this, you can witness component activation and deactivation, as well as schedule an
      effect.

      Most lifecycle events are run at the end of every frame, so incremental updates and
      DOM updates will be run before those lifecycle events.

      [before_display] lifecycle events are the exception; they are run before the view is
      updated. Changes made in [before_display] effects can trigger other [before_display]
      effects to trigger in the same frame, but each individual effect can only run at
      most once per frame.

      Lifecycle events are run in the following order:
      1. before_display
      2. (... incremental/DOM updates ...)
      3. on_deactivate
      4. on_activate
      5. after_display *)
  val lifecycle
    :  here:[%call_pos]
    -> ?on_activate:unit Effect.t t
    -> ?on_deactivate:unit Effect.t t
    -> ?before_display:unit Effect.t t
    -> ?after_display:unit Effect.t t
    -> local_ graph
    -> unit

  (** Just like [lifecycle] except that each effect is optional *)
  val lifecycle'
    :  here:[%call_pos]
    -> ?on_activate:unit Effect.t option t
    -> ?on_deactivate:unit Effect.t option t
    -> ?before_display:unit Effect.t option t
    -> ?after_display:unit Effect.t option t
    -> local_ graph
    -> unit

  (** [before_display] will schedule an effect right before the dom is updated. *)
  val before_display : here:[%call_pos] -> unit Effect.t t -> local_ graph -> unit

  (** Just like [before_display] except that the effect is optional *)
  val before_display' : here:[%call_pos] -> unit Effect.t option t -> local_ graph -> unit

  (** [after_display] will schedule an effect at the end of every frame. *)
  val after_display : here:[%call_pos] -> unit Effect.t t -> local_ graph -> unit

  (** Just like [after_display] except that the effect is optional *)
  val after_display' : here:[%call_pos] -> unit Effect.t option t -> local_ graph -> unit

  (** [wait_before_display] gives you an effect that will block until the beginning of the
      next frame. *)
  val wait_before_display : here:[%call_pos] -> local_ graph -> unit Effect.t t

  (** [wait_after_display] gives you an effect that will block until the end of the next
      frame. *)
  val wait_after_display : here:[%call_pos] -> local_ graph -> unit Effect.t t

  module Poll : sig
    module Starting : sig
      type ('o, 'r) t

      val empty : ('o, 'o option) t
      val initial : 'o -> ('o, 'o) t
    end

    (** [Bonsai.Edge.Poll.effect_on_change] will invoke the [~effect] function every time
        that the input ['a Bonsai.t] changes. The result of the effect function is stored
        and returned as a new [Bonsai.t].

        The main feature of [Bonsai.Edge.Poll.effect_on_change] is that it guarantees that
        the result from an effect that are scheduled later than another effect will take
        priority, even if the effects complete out of order. *)
    val effect_on_change
      :  here:[%call_pos]
      -> ?sexp_of_input:('a -> Sexp.t)
      -> ?sexp_of_result:('o -> Sexp.t)
      -> equal_input:('a -> 'a -> bool)
      -> ?equal_result:('o -> 'o -> bool)
      -> ('o, 'r) Starting.t
      -> 'a t
      -> effect:('a -> 'o Effect.t) t
      -> local_ graph
      -> 'r t

    (** [manual_refresh] is a stateful component for storing the result of an effect. The
        most recent return value is returned inside a [Bonsai.t] alongside a
        [unit Effect.t Bonsai.t] that contains an effect for kicking off a new effect. *)
    val manual_refresh
      :  here:[%call_pos]
      -> ?sexp_of_model:('o -> Sexp.t)
      -> ?equal:('o -> 'o -> bool)
      -> ('o, 'r) Starting.t
      -> effect:'o Effect.t t
      -> local_ graph
      -> 'r t * unit Effect.t t
  end
end

module Memo : sig
  (** The [Memo] module can be used to share a computation between multiple components,
      meaning that if the shared computation is stateful, then the users of that
      computation will see the same state.

      The way that [Memo] differs from just using [let%sub] on a computation and then
      passing the resulting [Value.t] down to its children is twofold:
      - The shared computation is not made active until it's actually requested by another
        component
      - Knowledge of any inputs to component are be deferred to "lookup time", when
        components request an instance of the component.

      Shared computations are refcounted, so when the last user of a memoized component
      deactivates, the shared component is deactivated as well. *)

  type 'a bonsai_t := 'a t
  type ('input, 'result) t

  (** Creates a memo instance that can be used by calling [lookup] *)
  val create
    :  here:[%call_pos]
    -> ('input, 'cmp) Comparator.Module.t
    -> f:('input bonsai_t -> local_ graph -> 'result bonsai_t)
    -> local_ graph
    -> ('input, 'result) t bonsai_t

  (** Requests an instance of the shared computation for a given ['input] value. If an
      instance doesn't already exist, it will request a new computation, which results in
      [none] being returned for a brief period of time, after which it'll return a [Some]
      containing the result of that computation *)
  val lookup
    :  here:[%call_pos]
    -> ('input, 'result) t bonsai_t
    -> 'input bonsai_t
    -> local_ graph
    -> 'result option bonsai_t

  type ('query, 'response) responses =
    | T : ('query, 'response, 'cmp) Map.t -> ('query, 'response) responses

  (** Gets all existing entries for each query of the Memo as a [Map.t]. Useful if you
      want to inspect data from your Memo without actually triggering a computation for a
      given query *)
  val responses : ('i, 'r) t -> ('i, 'r) responses
end

module Effect_throttling : sig
  module Poll_result : sig
    type 'a t =
      | Aborted
      (** [Aborted] indicates that the effect was aborted before it even started. If an
          effect starts, then it should complete with some kind of result - [Effect] does
          not support cancellation in general. *)
      | Finished of 'a
      (** [Finished x] indicates that an effect successfully completed with value x. *)
    [@@deriving sexp, equal]

    (** Collapses values of type ['a Or_error.t t] a plain Or_error.t, where the Aborted
        case is transformed into an error.

        The [tag_s] parameter can be used to add additional info to the error. *)
    val collapse_to_or_error : ?tag_s:Sexp.t lazy_t -> 'a Or_error.t t -> 'a Or_error.t

    (** Like [collapse_to_or_error], but transforms a function that returns an
        ['a Or_error.t t] instead of just the value. *)
    val collapse_fun_to_or_error
      :  ?sexp_of_input:('a -> Sexp.t)
      -> ('a -> 'b Or_error.t t Effect.t)
      -> ('a -> 'b Or_error.t Effect.t)
  end

  (** Transforms an input effect into a new effect that enforces that invariant that at
      most one instance of the effect is running at once. Attempting to run the effect
      while a previous run is still ongoing will cause the new effect to be enqueued. Any
      previously enqueued item gets kicked out, thus maintaining the invariant that at
      most one effect will be enqueued. (this is important so that things like RPCs calls
      don't pile up)

      CAUTION: This computation assumes that the input effect will always complete. If a
      run of the effect raises, no more runs will ever get executed, since they will all
      be waiting for the one that raised to complete. *)
  val poll
    :  here:[%call_pos]
    -> ('a -> 'b Effect.t) t
    -> local_ graph
    -> ('a -> 'b Poll_result.t Effect.t) t
end

module Dynamic_scope : sig
  (** This module implements dynamic variable scoping. Once a dynamic variable is created,
      you can store values in it, and lookup those same values. A lookup will find the
      nearest-most parent "unreverted" [set] call where a "set" can be "reverted" with
      [set']'s [revert]. *)

  type 'a bonsai_t := 'a t
  type 'a t

  (** Creates a new variable for use with the rest of the functions. It is critically
      important that the exact same [Dynamic_scope.t] is used in calls to [set] and the
      corresponding [lookup]. *)
  val create : ?sexp_of:('a -> Sexp.t) -> name:string -> fallback:'a -> unit -> 'a t

  (** Creates a variable which is derived from another. Typically this is used to project
      out a field of another dynamic variable which contains a record. *)
  val derived
    :  ?sexp_of:('a -> Sexp.t)
    -> 'b t
    -> get:('b -> 'a)
    -> set:('b -> 'a -> 'b)
    -> 'a t

  (** Given a ['a Dynamic_scope.t] and a ['a Bonsai.t] evaluate the [~inside] function
      that now has access to the value via the [lookup] function. *)
  val set
    :  here:[%call_pos]
    -> 'a t
    -> 'a bonsai_t
    -> inside:(local_ graph -> 'r bonsai_t)
    -> local_ graph
    -> 'r bonsai_t

  type revert =
    { revert : 'a. (local_ graph -> 'a bonsai_t) -> (local_ graph -> 'a bonsai_t) }

  (** like [set] but with the ability to revert the value in sub-computations. *)
  val set'
    :  here:[%call_pos]
    -> 'a t
    -> 'a bonsai_t
    -> f:(revert -> local_ graph -> 'r bonsai_t)
    -> local_ graph
    -> 'r bonsai_t

  (** Lookup attempts to find the value inside the nearest scope, but if there isn't one,
      it falls back to default specified in [create]. *)
  val lookup : here:[%call_pos] -> 'a t -> local_ graph -> 'a bonsai_t

  (** [modify] can be used to change the current value of a dynamically scoped variable. *)
  val modify
    :  here:[%call_pos]
    -> 'a t
    -> change:('a bonsai_t -> 'a bonsai_t)
    -> f:(revert -> local_ graph -> 'r bonsai_t)
    -> local_ graph
    -> 'r bonsai_t
end

module Incr : sig
  val value_cutoff
    :  here:[%call_pos]
    -> 'a t
    -> equal:('a -> 'a -> bool)
    -> local_ graph
    -> 'a t

  (** compute a value using the lower-level [Incremental] library *)
  val compute
    :  here:[%call_pos]
    -> 'a t
    -> f:('a Incr.t -> 'b Incr.t)
    -> local_ graph
    -> 'b t

  (** Compute an incremental value from a Time_source.t *)
  val with_clock
    :  here:[%call_pos]
    -> f:(Time_source.t -> 'a Incr.t)
    -> local_ graph
    -> 'a t

  val to_value : here:[%call_pos] -> 'a Incr.t -> 'a t
end

(** [assoc] is used to build a new instance of a Bonsai component for each element of a
    map.

    This function is very similar to [Map.mapi] or [Incr_map.mapi'], and for good reason!
    It is doing the same thing (taking a map and a function and returning a new map with
    the function applied to every key-value pair), but this function does it with the
    Bonsai values, which means that the computation is done incrementally and also
    maintains a state machine for every key-value pair. *)
val assoc
  :  here:[%call_pos]
  -> ('k, 'cmp) Comparator.Module.t
  -> ('k, 'v, 'cmp) Map.t t
  -> f:('k t -> 'v t -> local_ graph -> 'a t)
  -> local_ graph
  -> ('k, 'a, 'cmp) Map.t t

(** Exactly like [assoc] but will automatically pack multiple [Bonsai.t] produced inside
    the [f] closure into a tuple. See documentation for [Autopack.t] for more info. *)
val assoc_n
  :  here:[%call_pos]
  -> ('k, 'cmp) Comparator.Module.t
  -> ('k, 'v, 'cmp) Map.t t
  -> f:('k t -> 'v t -> local_ graph -> 'unpacked)
  -> n:('packed, 'unpacked) Autopack.t
  -> local_ graph
  -> ('k, 'packed, 'cmp) Map.t t

(** [all_map] is like [assoc] but is only usable when the input map is constant. *)
val all_map
  :  here:[%call_pos]
  -> ('k, local_ graph -> 'v t, 'cmp) Map.t
  -> local_ graph
  -> ('k, 'v, 'cmp) Map.t t

(** Like [assoc] except that the input value is a Set instead of a Map. *)
val assoc_set
  :  here:[%call_pos]
  -> ('key, 'cmp) Comparator.Module.t
  -> ('key, 'cmp) Set.t t
  -> f:('key t -> local_ graph -> 'result t)
  -> local_ graph
  -> ('key, 'result, 'cmp) Map.t t

(** Exactly like [assoc_set] but will automatically pack multiple [Bonsai.t] produced
    inside the [f] closure into a tuple. See documentation for [Autopack.t] for more info. *)
val assoc_set_n
  :  here:[%call_pos]
  -> ('key, 'cmp) Comparator.Module.t
  -> ('key, 'cmp) Set.t t
  -> f:('key t -> local_ graph -> 'unpacked)
  -> n:('packed, 'unpacked) Autopack.t
  -> local_ graph
  -> ('key, 'packed, 'cmp) Map.t t

(** Like [assoc] except that the input value is a list instead of a Map. The output list
    is in the same order as the input list. This function performs O(n log(n)) work (where
    n is the length of the list) any time that anything in the input list changes, so it
    may be quite slow with large lists. *)
val assoc_list
  :  here:[%call_pos]
  -> ('key, _) Comparator.Module.t
  -> 'a list t
  -> get_key:('a -> 'key)
  -> f:('key t -> 'a t -> local_ graph -> 'b t)
  -> local_ graph
  -> [ `Duplicate_key of 'key | `Ok of 'b list ] t

(** Exactly like [assoc_list] but will automatically pack multiple [Bonsai.t] produced
    inside the [f] closure into a tuple. See documentation for [Autopack.t] for more info. *)
val assoc_list_n
  :  here:[%call_pos]
  -> ('key, _) Comparator.Module.t
  -> 'a list t
  -> get_key:('a -> 'key)
  -> f:('key t -> 'a t -> local_ graph -> 'unpacked)
  -> n:('packed, 'unpacked) Autopack.t
  -> local_ graph
  -> [ `Duplicate_key of 'key | `Ok of 'packed list ] t

module Time_source = Time_source

module Debug : sig
  (** [on_change v ~f] executes the function [f] every time that [v] is recomputed. *)
  val on_change : here:[%call_pos] -> 'a t -> f:('a -> unit) -> local_ graph -> unit

  (** like [on_change], but specialized for printing a sexp of the value that you are
      watching. *)
  val on_change_print_s
    :  here:[%call_pos]
    -> 'a t
    -> ('a -> Sexp.t)
    -> local_ graph
    -> unit

  (** Builds a graphviz dot file string for the component *)
  val to_dot : ?pre_process:bool -> (local_ graph -> 'a t) -> string

  (** Counts the number of nodes in the static Bonsai graph *)
  val bonsai_node_counts
    :  ?pre_process:bool
    -> (local_ graph -> 'a t)
    -> Skeleton.Counts.t

  val enable_incremental_annotations : unit -> unit
  val disable_incremental_annotations : unit -> unit

  (** Wrapping [watch_computation] around a computation will add informative print
      statements every time that a value defined outside the closue - and used _inside_
      the closure - is updated. This can be useful to debug why a component is being
      updated.

      By default, calls to [watch_computation] are no-ops, and must be enabled manually
      with external tools. This is so you can leave calls to this function in production
      builds without impacting performance until you start debugging.

      [log_model_before]: Will log a state machine's model before apply_action/reset is
      called. Uses the sexp_of_model function passed to the state machine, or
      sexp_of_opaque if no sexp function is provided. (Default: false)

      [log_model_after]: Will log a state machine's model after apply_action/reset is
      applied to it. Uses the sexp_of_model function passed to the state machine, or
      sexp_of_opaque if no sexp function is provided. (Default: false)

      [log_action]: Logs the action applied to the state machine. Uses the sexp_of_action
      function passed to the state machine, or sexp_of_opaque if no sexp function is
      provided. (Default: false)

      [log_incr_info]: Will log a state machine's model after apply_action/reset is
      applied to it. Uses the sexp_of_model function passed to the state machine, or
      sexp_of_opaque if no sexp function is provided. (Default: false)

      [log_watcher_positions]: Logs the source code positions of the Computation_watcher
      nodes that are relevant to the current change. The node nearest to the change is at
      the top (Default: true)

      [log_dependency_definition_position]: Logs the source code position of the
      Computation node that updated and caused the watched computation to update.
      (Default: true)

      [label]: Prefixes the watcher position in the list of watchers for easier
      identification of individual watchers. Will not show up if [log_watcher_positions]
      is set to false (Default: None) *)
  val watch_computation
    :  here:[%call_pos]
    -> ?log_model_before:bool
    -> ?log_model_after:bool
    -> ?log_action:bool
    -> ?log_incr_info:bool
    -> ?log_watcher_positions:bool
    -> ?log_dependency_definition_position:bool
    -> ?label:string
    -> f:(local_ graph -> 'a t)
    -> local_ graph
    -> 'a t

  (** [memo_subscribers] prints the internal state for each query. *)
  val memo_subscribers : ('query, _) Memo.t -> 'query Path.Map.t
end

module Path : sig
  (** A [Bonsai.Path.t] is a value that records the path through the bonsai graph that was
      taken to reach the requesting component. This means that every call to [Bonsai.path]
      will yield a distinct and reproducable [Bonsai.Path.t], which you can use as keys,
      or to generate stable unique ids. *)

  type t = Path.t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t

  (** Converts the path to a "unique" string that contains only lowercase letters and
      underscores. This makes it viable for e.g. HTML ids.

      The uniqueness of this string depends on the uniqueness of the sexp function for any
      modules that are being used in "assoc". The invariant that must be upheld by those
      modules is the following:

      [a != b] implies [sexp_of a != sexp_of b] *)
  val to_unique_identifier_string : t -> string
end

(** Fetches the current [Path.t] *)
val path : here:[%call_pos] -> local_ graph -> Path.t t

(** Fetches the current [Path.t] and converts it to a string using
    [Path.to_unique_identifier_string] *)
val path_id : here:[%call_pos] -> local_ graph -> string t

val arr1 : here:[%call_pos] -> local_ graph -> 'a t -> f:('a -> 'b) -> 'b t
val arr2 : here:[%call_pos] -> local_ graph -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

module Effect = Ui_effect

module Let_syntax : sig
  val ( >>| ) : here:[%call_pos] -> 'a t -> ('a -> 'b) -> 'b t
  val return : here:[%call_pos] -> 'a -> 'a t

  module Let_syntax : sig
    val map : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
    val map2 : here:[%call_pos] -> 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val both : here:[%call_pos] -> 'a t -> 'b t -> ('a * 'b) t
    val arr : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
    val return : 'a t -> 'a t
    val cutoff : here:[%call_pos] -> 'a t -> equal:('a -> 'a -> bool) -> 'a t

    val switch
      :  here:Source_code_position.t
      -> match_:int t
      -> branches:int
      -> with_:local_ (int -> 'a t)
      -> 'a t

    val sub : here:[%call_pos] -> 'a -> f:local_ ('a -> 'b) -> 'b
    val delay : here:[%call_pos] -> f:(local_ graph -> 'a t) -> local_ graph -> 'a t

    include Mapn with type 'a t := 'a t
    include Arrn with type 'a t := 'a t
  end
end

include
  Map_and_set0_intf.Output
  with type 'a Value.t := 'a t
   and type 'a Computation.t := local_ graph -> 'a t
   and module Value := Value
   and module Computation := Computation

(** Delays the computation in [f] until it is needed. One use case for this is delaying
    large [match%sub] branches that might not initially be in use:

    {[
      match%sub x with
      | A -> Bonsai.delay ~f:terrible_computation_1 graph
      | B -> Bonsai.delay ~f:terrible_computation_2 graph
      | C -> Bonsai.delay ~f:terrible_computation_3 graph
    ]}

    Here, if [x] initially has the value [A], then only [terrible_computation_1] will be
    computed at first, while [terrible_computation_2] and [terrible_computation_3] will be
    put off until [x] changes to [B] or [C], respectively.

    Note that [f] will be called at most once.

    For recursive components, see [fix]. *)
val delay : here:[%call_pos] -> f:(local_ graph -> 'a t) -> local_ graph -> 'a t

module Expert : sig
  val thunk : here:[%call_pos] -> f:(unit -> 'a) -> local_ graph -> 'a t

  (** [assoc_on] is similar to [assoc], but allows the model to be keyed differently than
      the input map. This comes with a few caveats:

      - Inputs whose keys map to the same [model_key] will share the same model.
      - The result of [get_model_key] is used in a bind, so it is expensive when it
        changes.

      [assoc] should almost always be used instead. Consider whether you really need the
      additional power before reaching for this function. *)
  val assoc_on
    :  here:[%call_pos]
    -> ('io_key, 'io_cmp) Comparator.Module.t
    -> ('model_key, 'model_cmp) Comparator.Module.t
    -> ('io_key, 'data, 'io_cmp) Core.Map.t t
    -> get_model_key:('io_key -> 'data -> 'model_key)
    -> f:('io_key t -> 'data t -> local_ graph -> 'result t)
    -> local_ graph
    -> ('io_key, 'result, 'io_cmp) Core.Map.t t

  module Var : sig
    type 'a bonsai := 'a t

    (** A [Var.t] represents a ref to some global state, which can be used as an input to
        an incremental Bonsai computation.

        The most common use case of [Var.t]s is in tests, so that test inputs can be set
        outside of a Bonsai context. *)
    type 'a t

    (** Creates a new [Var.t] with an initial value. *)
    val create : 'a -> 'a t

    (** Provides incremental, read-only access to [t] by producing a {!Bonsai.t}. *)
    val value : here:[%call_pos] -> 'a t -> 'a bonsai

    (** Updates the value inside of [t]. [f] is given the previous value of [t] so that
        you can reuse parts of the value if applicable. *)
    val update : 'a t -> f:('a -> 'a) -> unit

    (** Sets the value inside of [t]. *)
    val set : here:[%call_pos] -> 'a t -> 'a -> unit

    (** Gets the value inside of [t]. *)
    val get : 'a t -> 'a

    (** Retrieves the underlying ['a t] Ui_incr.t var. *)
    val incr_var : 'a t -> 'a Ui_incr.Var.t
  end

  module For_bonsai_internal : sig
    val set_perform_on_exception : (exn -> unit) -> unit
  end
end

(** These functions are here to provide the basis for [bonsai_proc] which wants versions
    of these functions that don't have calls to [split ~here] in them. *)
module For_proc : sig
  module type Map_and_set0_output = Map_and_set0_intf.Output

  val arr1 : here:[%call_pos] -> local_ graph -> 'a t -> f:('a -> 'b) -> 'b t

  val arr2
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> f:('a -> 'b -> 'c)
    -> 'c t

  val arr3
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> 'c t
    -> f:('a -> 'b -> 'c -> 'd)
    -> 'd t

  val arr4
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> f:('a -> 'b -> 'c -> 'd -> 'e)
    -> 'e t

  val arr5
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f)
    -> 'f t

  val arr6
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
    -> 'g t

  val arr7
    :  here:[%call_pos]
    -> local_ graph
    -> 'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> 'g t
    -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h)
    -> 'h t

  val value_cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t
  val value_map : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
  val conceal_value : 'a Value.t -> 'a t

  val state
    :  here:[%call_pos]
    -> ?reset:('model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> 'model
    -> local_ graph
    -> ('model * ('model -> unit Effect.t)) t

  val state'
    :  here:[%call_pos]
    -> ?reset:('model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> 'model
    -> local_ graph
    -> ('model * (('model -> 'model) -> unit Effect.t)) t

  val state_opt
    :  here:[%call_pos]
    -> ?reset:('model option -> 'model option)
    -> ?default_model:'model
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> unit
    -> local_ graph
    -> ('model option * ('model option -> unit Effect.t)) t

  val toggle
    :  here:[%call_pos]
    -> default_model:bool
    -> local_ graph
    -> (bool * unit Effect.t) t

  module Toggle : sig
    type t =
      { state : bool
      ; set_state : bool -> unit Effect.t
      ; toggle : unit Effect.t
      }
  end

  val toggle' : here:[%call_pos] -> default_model:bool -> local_ graph -> Toggle.t t

  val state_machine
    :  here:[%call_pos]
    -> ?reset:('model, 'action, unit) resetter
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         (('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model)
    -> unit
    -> local_ graph
    -> ('model * ('action -> unit Effect.t)) t

  val state_machine_with_input
    :  here:[%call_pos]
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?reset:('model, 'action, unit) resetter
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         (('action, unit) Apply_action_context.t
          -> 'input Computation_status.t
          -> 'model
          -> 'action
          -> 'model)
    -> 'input t
    -> local_ graph
    -> ('model * ('action -> unit Effect.t)) t

  val actor
    :  here:[%call_pos]
    -> ?reset:(('action, 'return) Apply_action_context.t -> 'model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:
         (('action, 'return) Apply_action_context.t
          -> 'model
          -> 'action
          -> 'model * 'return)
    -> unit
    -> local_ graph
    -> ('model * ('action -> 'return Effect.t)) t

  val actor_with_input
    :  here:[%call_pos]
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?reset:(('action, 'return) Apply_action_context.t -> 'model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> recv:
         (('action, 'return) Apply_action_context.t
          -> 'input Computation_status.t
          -> 'model
          -> 'action
          -> 'model * 'return)
    -> 'input t
    -> local_ graph
    -> ('model * ('action -> 'return Effect.t)) t

  val wrap
    :  here:[%call_pos]
    -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         (('action, unit) Apply_action_context.t
          -> 'result Computation_status.t
          -> 'model
          -> 'action
          -> 'model)
    -> f:('model t -> ('action -> unit Effect.t) t -> local_ graph -> 'result t)
    -> unit
    -> local_ graph
    -> 'result t

  val switch
    :  here:[%call_pos]
    -> match_:int t
    -> branches:int
    -> with_:(int -> local_ graph -> 'a t)
    -> local_ graph
    -> 'a t

  val with_model_resetter
    :  here:[%call_pos]
    -> (local_ graph -> 'a t)
    -> local_ graph
    -> ('a * unit Effect.t) t

  val with_model_resetter'
    :  here:[%call_pos]
    -> (reset:unit Effect.t t -> local_ graph -> 'a t)
    -> local_ graph
    -> 'a t

  val on_change
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> trigger:[ `Before_display | `After_display ]
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a -> unit Effect.t) t
    -> local_ graph
    -> unit t

  val on_change'
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> trigger:[ `Before_display | `After_display ]
    -> equal:('a -> 'a -> bool)
    -> 'a t
    -> callback:('a option -> 'a -> unit Effect.t) t
    -> local_ graph
    -> unit t

  val lifecycle
    :  here:[%call_pos]
    -> ?on_activate:unit Effect.t t
    -> ?on_deactivate:unit Effect.t t
    -> ?before_display:unit Effect.t t
    -> ?after_display:unit Effect.t t
    -> unit
    -> local_ graph
    -> unit t

  val lifecycle'
    :  here:[%call_pos]
    -> ?on_activate:unit Effect.t option t
    -> ?on_deactivate:unit Effect.t option t
    -> ?before_display:unit Effect.t option t
    -> ?after_display:unit Effect.t option t
    -> unit
    -> local_ graph
    -> unit t

  val before_display : here:[%call_pos] -> unit Effect.t t -> local_ graph -> unit t

  val before_display'
    :  here:[%call_pos]
    -> unit Effect.t option t
    -> local_ graph
    -> unit t

  val after_display : here:[%call_pos] -> unit Effect.t t -> local_ graph -> unit t

  val after_display'
    :  here:[%call_pos]
    -> unit Effect.t option t
    -> local_ graph
    -> unit t

  val manual_refresh
    :  here:[%call_pos]
    -> ?sexp_of_model:('o -> Sexp.t)
    -> ?equal:('o -> 'o -> bool)
    -> ('o, 'r) Edge.Poll.Starting.t
    -> effect:'o Effect.t t
    -> local_ graph
    -> ('r * unit Effect.t) t

  val debug_on_change
    :  here:[%call_pos]
    -> 'a t
    -> f:('a -> unit)
    -> local_ graph
    -> unit t

  val debug_on_change_print_s
    :  here:[%call_pos]
    -> 'a t
    -> ('a -> Sexp.t)
    -> local_ graph
    -> unit t

  val lazy_ : here:[%call_pos] -> (local_ graph -> 'a t) lazy_t -> local_ graph -> 'a t

  val narrow
    :  here:[%call_pos]
    -> ('a * ('b -> unit Effect.t)) t
    -> get:('a -> 'c)
    -> set:('a -> 'd -> 'b)
    -> local_ graph
    -> ('c * ('d -> unit Effect.t)) t

  val narrow_via_field
    :  here:[%call_pos]
    -> ('a * ('a -> unit Effect.t)) t
    -> ('a, 'b) Field.t
    -> local_ graph
    -> ('b * ('b -> unit Effect.t)) t
end

module Conv : sig
  val handle
    :  here:[%call_pos]
    -> f:(local_ graph -> 'a t)
    -> local_ graph
    -> 'a Computation.t

  val top_level_handle : here:[%call_pos] -> (local_ graph -> 'a t) -> 'a Computation.t
  val perform : here:[%call_pos] -> local_ graph -> 'a Computation.t -> 'a t
  val reveal_value : 'a t -> 'a Value.t
  val conceal_value : 'a Value.t -> 'a t

  val isolated
    :  local_ graph
    -> here:Source_code_position.t
    -> f:local_ (unit -> 'a Value.t)
    -> 'a Computation.t
end
