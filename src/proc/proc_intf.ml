open! Core
open! Import

module type S = sig
  module type Enum = Module_types.Enum

  (** The functions found in this module are focused on the manipulation of values of type
      ['a Computation.t] and ['a Value.t]. There are fine descriptions of these types
      below and how to use them, but since it's so common to convert between the two, here
      is a cheat-sheet matrix for converting between values of different types:

      {v
    | Have \ Want      | 'a Value.t             | 'a Computation.t |
    |------------------+------------------------+------------------|
    | 'a               | let v = Value.return a | let c = const a  |
    | 'a Value.t       |                        | let c = read v   |
    | 'a Computation.t | let%sub v = c          |                  |
      v} *)

  module Value : sig
    (** A value of type ['a Value.t] represents a value that may change during the
        lifetime of the program. For those familiar with the [Incremental] library, this
        type is conceptually very similar to [Incr.t]. The main method by which you
        acquire values of type [Value.t] is by using the [let%sub] syntax extension.

        {[
          val c : int Computation.t

          let%sub x = c in
          (* [x] has type [int Value.t] here *)
        ]}

        In the example above, we run a computation [c] and store the result of that
        computation in [x] which has type [Value.t].

        [Value.t] is an applicative, which means that you can combine multiple [Value]s
        into one by using [Let_syntax]:

        {[
          val a : int Value.t
          val b : int Value.t

          let open Let_syntax in
          let%map a = a and b = b in
          a + b
        ]} *)
    type 'a t

    include Applicative.S with type 'a t := 'a t
    include Mapn with type 'a t := 'a t

    (** A [Value.t] transformed by [cutoff] will only trigger changes on its dependents
        when the equality of the contained value has changed.

        Immediate nesting of cutoff nodes are combined into a single cutoff node whose
        equality function is true when any of the composed nodes is true and is false when
        all of the composed nodes are false. They're "or'ed together". *)
    val cutoff : here:[%call_pos] -> 'a t -> equal:('a -> 'a -> bool) -> 'a t

    (** flips the option position in a ['a Value.t option] into an ['a option Value.t].
        It's useful for optional args that take values. *)
    val transpose_opt : 'a t option -> 'a option t
  end

  module Computation : sig
    (** A value of type ['a Computation.t] represents a computation which produces a value
        that may change during the lifetime of a program, and the value may be influenced
        by the internal state of that computation.

        The same ['a Computation.t] can be used in multiple places in a program, and these
        uses will {e not} share the same state, nor will they share the work performed by
        the computation.

        In this normal OCaml code, if we see the same function being called multiple
        times:

        {[
          let a = f () in
          let b = f () in
          a + b
        ]}

        You would not be surprised to know that if [f] has side-effects (maybe printing to
        the console), then those side-effects happen twice because [f] was called twice.

        Similarly, if we wrote the code this way:

        {[
          let a = f () in
          let b = a in
          a + b
        ]}

        You would (correctly) expect that the side-effect only happens once, when
        computing [a]. In these examples, the {e code} [f ()] is analogous to
        [_ Computation.t]. If you want to have two separate values whose computations
        maintain separate state, you would use two instances of "let%sub" to bind them
        separately:

        {[
          val some_computation : int Computation.t
          val add : int Value.t -> int Value.t -> int Computation.t

          let open Let_syntax in
          let%sub a = some_computation in
          let%sub b = some_computation in
          add a b
        ]}

        Here, [a] and [b] can take on different values depending on the states of the
        computations that produce them.

        However, if you want to use just one value in multiple places, only use [let%sub]
        once:

        {[
          let open Let_syntax in
          let%sub a = some_computation in
          let b = a in
          add a b
        ]}

        Here, [a] and [b] always take on the same value. *)
    type 'a t

    include Applicative.S with type 'a t := 'a t

    (** Similar to [all] which pulls the computation outside of a list, [all_map] does the
        same, but with the data in a map. This can be a useful replacement for [assoc] in
        scenarios where the map is a constant size. *)
    val all_map : here:[%call_pos] -> ('k, 'v t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t t

    (** The analog of [List.reduce_balanced] for computations, but with [f] operating on
        values instead of the computations themselves *)
    val reduce_balanced : 'a t list -> f:('a Value.t -> 'a Value.t -> 'a t) -> 'a t option

    val fold_right
      :  'a t list
      -> f:('a Value.t -> 'acc Value.t -> 'acc t)
      -> init:'acc Value.t
      -> 'acc t

    module Let_syntax : sig
      val return : here:[%call_pos] -> 'a -> 'a t

      include Import.Applicative.Applicative_infix with type 'a t := 'a t

      module Let_syntax : sig
        val return : here:[%call_pos] -> 'a -> 'a t
        val map : here:[%call_pos] -> 'a t -> f:('a -> 'b) -> 'b t
        val both : here:[%call_pos] -> 'a t -> 'b t -> ('a * 'b) t

        include Mapn with type 'a t := 'a t
      end
    end

    include Mapn with type 'a t := 'a t
  end

  module Effect = Ui_effect

  module For_open : sig
    module Computation = Computation
    module Effect = Effect
    module Value = Value
  end

  module Var : sig
    (** A [Var.t] is the primary method for making data obtained outside of Bonsai (maybe
        via an RPC) accessible inside a Bonsai application. *)
    type 'a t

    (** Creates a new [Var.t] with an initial value. *)
    val create : 'a -> 'a t

    (** Updates the value inside of [t]. [f] is given the previous value of [t] so that
        you can reuse parts of the value if applicable *)
    val update : 'a t -> f:('a -> 'a) -> unit

    (** Sets the value inside of [t]. *)
    val set : here:[%call_pos] -> 'a t -> 'a -> unit

    (** Gets the value inside of [t]. *)
    val get : 'a t -> 'a

    (** Provides read-only access to [t] by producing a {!Value.t} which is used inside of
        a Bonsai computation. *)
    val value : here:[%call_pos] -> 'a t -> 'a Value.t

    (** Retrieves the underlying ['a t] Ui_incr.t var. *)
    val incr_var : 'a t -> 'a Ui_incr.Var.t
  end

  (** Converts a [Value.t] to a [Computation.t]. Unlike most Computations, the
      [Computation.t] returned by [read] can be used in multiple locations without
      maintaining multiple copies of any models or building duplicate incremental graphs.

      [read] is most commonly used in the final expression of a [let%sub] chain, like so:

      {[
        fun i ->
          let%sub a = f i in
          let%sub b = g i in
          read
            (let%map a and b in
             a + b)
      ]}

      or to use some APIs that require [Computation.t] like so:

      {[
        val cond : bool Value.t
        val x : 'a Value.t
        val some_computation : 'a Computation.t

        let y = if_ cond ~then_:some_computation ~else_:(read x)

        val y : 'a Computation.t
      ]} *)
  val read : 'a Value.t -> 'a Computation.t

  (** Creates a [Computation.t] that provides a constant value. *)
  val const : here:[%call_pos] -> 'a -> 'a Computation.t

  (** Retrieves the path to the current computation as a string. This string is not
      human-readable, but can be used as an ID which is unique to this particular instance
      of a component. *)
  val path_id : here:[%call_pos] -> unit -> string Computation.t

  (** Lifts a regular OCaml function into one that takes a Value as input, and produces a
      Computation as output. *)
  val pure : here:[%call_pos] -> ('a -> 'b) -> 'a Value.t -> 'b Computation.t

  module Computation_status : sig
    (** Indicates whether a value is available, which depends on whether the computation
        in which it is computed is active or not. Most of the time values of this type are
        [Active], since it is unusual to interact with inactive computations.

        A computation is considered inactive if it resides in the inactive arm of a
        [match%sub] or in a removed entry of a [Bonsai.assoc]. *)
    type 'input t =
      | Active of 'input
      | Inactive
    [@@deriving sexp_of]
  end

  (** A frequently used state-machine is the trivial 'set-state' transition, where the
      action always replaces the value contained inside. This helper-function implements
      that state-machine, providing access to the current state, as well as an inject
      function that updates the state. *)
  val state
    :  here:[%call_pos]
    -> ?reset:('model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> 'model
    -> ('model * ('model -> unit Effect.t)) Computation.t

  (** Similar to [state], but stores an option of the model instead. [default_model] is
      optional and defaults to [None]. *)
  val state_opt
    :  here:[%call_pos]
    -> ?reset:('model option -> 'model option)
    -> ?default_model:'model
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> unit
    -> ('model option * ('model option -> unit Effect.t)) Computation.t

  (** Similar to [state], but the `set` function takes a function that calculates the new
      state from the previous state. *)
  val state'
    :  here:[%call_pos]
    -> ?reset:('model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> 'model
    -> ('model * (('model -> 'model) -> unit Effect.t)) Computation.t

  (** A bool-state which starts at [default_model] and flips whenever the returned effect
      is scheduled. *)
  val toggle
    :  here:[%call_pos]
    -> default_model:bool
    -> unit
    -> (bool * unit Effect.t) Computation.t

  module Toggle : sig
    type t =
      { state : bool
      ; set_state : bool -> unit Effect.t
      ; toggle : unit Effect.t
      }
  end

  (** Like [toggle], but also gives a handle to set the state directly *)
  val toggle' : here:[%call_pos] -> default_model:bool -> unit -> Toggle.t Computation.t

  module Apply_action_context : sig
    type ('action, 'response) t = ('action, 'response) Apply_action_context.t

    val inject : ('action, 'response) t -> 'action -> 'response Effect.t
    val schedule_event : _ t -> unit Effect.t -> unit
    val time_source : _ t -> Time_source.t
  end

  (** A constructor for [Computation.t] that models a simple state machine. 'model
      describes the states in the state machine, while 'action describes the transitions
      between states.

      [default_model] is the initial state for the state machine, and [apply_action]
      implements the transition function that looks at the current state and the requested
      transition, and produces a new state.

      (It is very common for ['action Apply_action_context.t] to be unused) *)
  val state_machine
    :  here:[%call_pos]
    -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         (('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model)
    -> unit
    -> ('model * ('action -> unit Effect.t)) Computation.t

  (** The same as {!state_machine}, but [apply_action] also takes an input from a
      [Value.t]. The input has type ['input Computation_status.t] instead of plain
      ['input] to account for the possibility that an action gets sent while the state
      machine is inactive. *)
  val state_machine_with_input
    :  here:[%call_pos]
    -> ?sexp_of_action:('action -> Sexp.t)
    -> ?reset:(('action, unit) Apply_action_context.t -> 'model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?equal:('model -> 'model -> bool)
    -> default_model:'model
    -> apply_action:
         (('action, unit) Apply_action_context.t
          -> 'input Computation_status.t
          -> 'model
          -> 'action
          -> 'model)
    -> 'input Value.t
    -> ('model * ('action -> unit Effect.t)) Computation.t

  (** Identical to [actor_with_input] but it takes 0 inputs instead of 1. *)
  val actor
    :  here:[%call_pos]
    -> ?reset:(('action, 'return) Apply_action_context.t -> 'model -> 'model)
         (** to learn more about [reset], read the docs on [with_model_resetter] *)
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
    -> ('model * ('action -> 'return Effect.t)) Computation.t

  (** [actor_with_input] is very similar to [state_machine_with_input], with two major
      exceptions:
      - the [apply-action] function for state-machine is renamed [recv], and it returns a
        "response", in addition to a new model.
      - the 2nd value returned by the component allows for the sender of an action to
        handle the effect and read the response.

      Because the semantics of this function feel like an actor system, we've decided to
      name the function accordingly. *)
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
    -> 'input Value.t
    -> ('model * ('action -> 'return Effect.t)) Computation.t

  (** Given a value containing the current state (like from a [Bonsai.state] or
      [Bonsai.state_machine]), [narrow] gives you access to a subset of the state and a
      setter for the subset of that type.

      For example, you could use [narrow] a state containing a record to the value and
      injection function for a single field. *)
  val narrow
    :  here:[%call_pos]
    -> ('a * ('input_action -> unit Effect.t)) Value.t
    -> get:('a -> 'b)
    -> set:('a -> 'output_action -> 'input_action)
    -> ('b * ('output_action -> unit Effect.t)) Computation.t

  (** Like [narrow], but [get] and [set] are implemented in terms of the given field. *)
  val narrow_via_field
    :  here:[%call_pos]
    -> ('a * ('a -> unit Effect.t)) Value.t
    -> ('a, 'b) Field.t
    -> ('b * ('b -> unit Effect.t)) Computation.t

  (** Given a first-class module that has no input (unit input type), and the default
      value of the state machine, [of_module] will create a [Computation] that produces
      values of that module's [Result.t] type. *)
  val of_module
    :  ?sexp_of_model:('m -> Sexp.t)
    -> ?equal:('m -> 'm -> bool)
    -> (unit, 'm, 'a, 'r) component_s
    -> default_model:'m
    -> 'r Computation.t

  (** The same as {!of_module}, but this one has an input type ['i]. Because input to the
      component is required, this function also expects a [Value.t] that provides its
      input. It is common for this function to be partially applied like so:

      {[
        val a : int Value.t
        val b : int Value.t

        let f = of_module_with_input (module struct ... end) ~default_model in
        let%sub a = f a in
        let%sub b = f b in
        ...
      ]}

      Where the [Value.t] values are passed in later. *)
  val of_module_with_input
    :  here:[%call_pos]
    -> ?sexp_of_model:('m -> Sexp.t)
    -> ('i, 'm, 'a, 'r) component_s
    -> ?equal:('m -> 'm -> bool)
    -> default_model:'m
    -> 'i Value.t
    -> 'r Computation.t

  (** The same as {!of_module_with_input} but with two inputs. *)
  val of_module2
    :  here:[%call_pos]
    -> ?sexp_of_model:('m -> Sexp.t)
    -> ('i1 * 'i2, 'm, 'a, 'r) component_s
    -> ?equal:('m -> 'm -> bool)
    -> default_model:'m
    -> 'i1 Value.t
    -> 'i2 Value.t
    -> 'r Computation.t

  (** [freeze] takes a Value.t and returns a computation whose output is frozen to be the
      first value that passed through the input. *)
  val freeze
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> ?equal:('a -> 'a -> bool)
    -> 'a Value.t
    -> 'a Computation.t

  (** [lazy_ c] produces a computation that only forces [c] when necessary. This can be
      used to delay large computations that might not initially be useful.

      For recursive computations, see [fix]. *)
  val lazy_ : here:[%call_pos] -> 'a Computation.t Lazy.t -> 'a Computation.t

  (** A fixed-point combinator for bonsai components. This is used to build recursive
      components like so:

      {[
        let my_recursive_component ~some_input =
          Bonsai.fix some_input ~f:(fun ~recurse some_input ->
            (* call [recurse] to instantiate a nested instance of the component *)
          )
      ]} *)
  val fix
    :  here:[%call_pos]
    -> 'input Value.t
    -> f:
         (recurse:('input Value.t -> 'result Computation.t)
          -> 'input Value.t
          -> 'result Computation.t)
    -> 'result Computation.t

  (** Like [fix], but for two arguments instead of just one. *)
  val fix2
    :  here:[%call_pos]
    -> 'a Value.t
    -> 'b Value.t
    -> f:
         (recurse:('a Value.t -> 'b Value.t -> 'result Computation.t)
          -> 'a Value.t
          -> 'b Value.t
          -> 'result Computation.t)
    -> 'result Computation.t

  (** [scope_model] allows you to have a different model for the provided computation,
      keyed by some other value.

      Suppose for example, that you had a form for editing details about a person. This
      form should have different state for each person. You could use scope_model, where
      the [~on] parameter is set to a user-id, and now when that value changes, the model
      for the other computation is set to the model for that particular user.

      [scope_model] also impacts lifecycle events; when [on] changes value, edge triggers
      like [on_activate] and [on_deactivate] will run *)
  val scope_model
    :  here:[%call_pos]
    -> ('a, _) Comparator.Module.t
    -> on:'a Value.t
    -> 'b Computation.t
    -> 'b Computation.t

  (** [most_recent_some] returns a value containing the most recent output of [f] for
      which it returned [Some]. If the input value has never contained a valid value, then
      the result is [None]. *)
  val most_recent_some
    :  here:[%call_pos]
    -> ?sexp_of_model:('b -> Sexp.t)
    -> equal:('b -> 'b -> bool)
    -> 'a Value.t
    -> f:('a -> 'b option)
    -> 'b option Computation.t

  (** [most_recent_value_satisfying] returns a value containing the most recent input
      value for which [condition] returns true. If the input value has never contained a
      valid value, then the result is [None]. *)
  val most_recent_value_satisfying
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a Value.t
    -> condition:('a -> bool)
    -> 'a option Computation.t

  (** [previous_value] returns the previous contents of the input value if it just
      changed, or the current contents of the value if it did not just change. Initially
      starts out as [None].

      Any values the input takes on while the output is inactive are ignored; any changes
      to the input are assumed to have occurred exactly when the component was
      re-activated. *)
  val previous_value
    :  here:[%call_pos]
    -> ?sexp_of_model:('a -> Sexp.t)
    -> equal:('a -> 'a -> bool)
    -> 'a Value.t
    -> 'a option Computation.t

  (** [assoc] is used to apply a Bonsai computation to each element of a map. This
      function signature is very similar to [Map.mapi] or [Incr_map.mapi'], and for good
      reason!

      It is doing the same thing (taking a map and a function and returning a new map with
      the function applied to every key-value pair), but this function does it with the
      Bonsai values, which means that the computation is done incrementally and also
      maintains a state machine for every key-value pair. *)
  val assoc
    :  here:[%call_pos]
    -> ('key, 'cmp) Comparator.Module.t
    -> ('key, 'data, 'cmp) Map.t Value.t
    -> f:('key Value.t -> 'data Value.t -> 'result Computation.t)
    -> ('key, 'result, 'cmp) Map.t Computation.t

  (** Like [assoc] except that the input value is a Set instead of a Map. *)
  val assoc_set
    :  here:[%call_pos]
    -> ('key, 'cmp) Comparator.Module.t
    -> ('key, 'cmp) Set.t Value.t
    -> f:('key Value.t -> 'result Computation.t)
    -> ('key, 'result, 'cmp) Map.t Computation.t

  (** Like [assoc] except that the input value is a list instead of a Map. The output list
      is in the same order as the input list.

      This function performs O(n log(n)) work (where n is the length of the list) any time
      that anything in the input list changes, so it may be quite slow with large lists. *)
  val assoc_list
    :  here:[%call_pos]
    -> ('key, _) Comparator.Module.t
    -> 'a list Value.t
    -> get_key:('a -> 'key)
    -> f:('key Value.t -> 'a Value.t -> 'b Computation.t)
    -> [ `Duplicate_key of 'key | `Ok of 'b list ] Computation.t

  (** [enum] is used for matching on a value and providing different behaviors on
      different values. The type of the value must be enumerable (there must be a finite
      number of possible values), and it must be comparable and sexpable.

      The rest of the parameters are named like you might expect from pattern-matching
      syntax, with [match_] taking the value to match on, and [with_] taking a function
      that choose which behavior to use. *)
  val enum
    :  here:[%call_pos]
    -> (module Enum with type t = 'k)
    -> match_:'k Value.t
    -> with_:('k -> 'a Computation.t)
    -> 'a Computation.t

  (** [wrap] wraps a Computation (built using [f]) and provides a model and injection
      function that the wrapped component can use. Especially of note is that the
      [apply_action] for this outer-model has access to the result value of the
      Computation being wrapped. ['result] is wrapped in a [Computation_status.t] so that
      you are able to reset the underlying model even if the computation is inactive *)
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
    -> f:('model Value.t -> ('action -> unit Effect.t) Value.t -> 'result Computation.t)
    -> unit
    -> 'result Computation.t

  (** [with_model_resetter] extends a computation with the ability to reset all of the
      models for components contained in that computation. The default behavior for a
      stateful component is to have its model set to the value provided by
      [default_model], though this behavior is overridable on a component-by-component
      basis by providing a value for the optional [reset] argument on stateful components. *)
  val with_model_resetter
    :  here:[%call_pos]
    -> 'a Computation.t
    -> ('a * unit Effect.t) Computation.t

  (** like [with_model_resetter], but makes the resetting effect available to the
      computation being wrapped. *)
  val with_model_resetter'
    :  here:[%call_pos]
    -> (reset:unit Effect.t Value.t -> 'a Computation.t)
    -> 'a Computation.t

  (** [yoink] is a function that takes a bonsai value and produces a computation producing
      an effect which fetches the current value out of the input. This can be useful
      inside of [let%bind.Effect] chains, where a value that you've closed over is stale
      and you want to witness a value after it's been changed by a previous effect.

      The ['a Computation_state.t] returned by the effect means that if the value was
      inactive at the time it got yoinked, then the effect will be unable to retrieve it. *)
  val yoink
    :  here:[%call_pos]
    -> 'a Value.t
    -> 'a Computation_status.t Effect.t Computation.t

  (** [sub] instantiates a computation and provides a reference to its results to [f] in
      the form of a [Value.t]. The main way to use this function is via the [let%sub]
      syntax extension. [here:[%call_pos]] is used by the Bonsai debugger to tie
      visualizations to precise source locations. *)
  val sub
    :  here:[%call_pos]
    -> 'a Computation.t
    -> f:('a Value.t -> 'b Computation.t)
    -> 'b Computation.t

  module Clock : sig
    (** Functions allowing for the creation of time-dependent computations in a testable
        way. *)

    (** The current time, updated at [tick_every] intervals. *)
    val approx_now
      :  here:[%call_pos]
      -> tick_every:Time_ns.Span.t
      -> unit
      -> Time_ns.t Computation.t

    module Before_or_after : sig
      type t = Ui_incr.Before_or_after.t =
        | Before
        | After
      [@@deriving sexp, equal]
    end

    (** Mirrors [Incr.Clock.at], which changes from [Before] to [After] at the specified
        time. *)
    val at : here:[%call_pos] -> Time_ns.t Value.t -> Before_or_after.t Computation.t

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
      -> Time_ns.Span.t Value.t
      -> unit Effect.t Value.t
      -> unit Computation.t

    (** An effect for fetching the current time. *)
    val get_current_time : here:[%call_pos] -> unit -> Time_ns.t Effect.t Computation.t

    (** The function in this computation produces an effect that completes after the
        specified amount of time. *)
    val sleep
      :  here:[%call_pos]
      -> unit
      -> (Time_ns.Span.t -> unit Effect.t) Computation.t

    (** Like [sleep], but waits until a specific time, rather than a time relative to now. *)
    val until : here:[%call_pos] -> unit -> (Time_ns.t -> unit Effect.t) Computation.t

    module Expert : sig
      (** The current time, update as frequently as possible. *)
      val now : here:[%call_pos] -> unit -> Time_ns.t Computation.t
    end
  end

  module Edge : sig
    (** All the functions in this module incorporate the concept of "edge-triggering",
        which is the terminology that we use to describe actions that occur when a value
        changes. *)

    (** When given a value and a callback, [on_change] and [on_change'] will watch the
        input variable and call the callback whenever the value changes.

        [callback] is also called when the component is initialized, passing in the first
        'a value that gets witnessed.

        These functions do not wait for previous calls to [callback] to complete before
        calling it again. *)
    val on_change
      :  here:[%call_pos]
      -> ?sexp_of_model:('a -> Sexp.t)
      -> trigger:[ `Before_display | `After_display ]
      -> equal:('a -> 'a -> bool)
      -> 'a Value.t
      -> callback:('a -> unit Effect.t) Value.t
      -> unit Computation.t

    (** The same as [on_change], but the callback function gets access to the previous
        value that was witnessed. *)
    val on_change'
      :  here:[%call_pos]
      -> ?sexp_of_model:('a -> Sexp.t)
      -> trigger:[ `Before_display | `After_display ]
      -> equal:('a -> 'a -> bool)
      -> 'a Value.t
      -> callback:('a option -> 'a -> unit Effect.t) Value.t
      -> unit Computation.t

    (** [lifecycle] is a way to detect when a computation becomes active, inactive, or an
        event is triggered after every rendering (roughly 60x / second). By depending on
        this function (with let%sub), you can install events that are scheduled on either
        case.

        When used, the events are scheduled in this order:
        - All deactivations
        - All activations
        - All "after-display"s

        and an "after-display" won't occur before an activation, or after a deactivation
        for a given computation. *)
    val lifecycle
      :  here:[%call_pos]
      -> ?on_activate:unit Effect.t Value.t
      -> ?on_deactivate:unit Effect.t Value.t
      -> ?before_display:unit Effect.t Value.t
      -> ?after_display:unit Effect.t Value.t
      -> unit
      -> unit Computation.t

    (** Like [lifecycle], but the events are optional values. If the event value is None
        when the action occurs, nothing will happen *)
    val lifecycle'
      :  here:[%call_pos]
      -> ?on_activate:unit Effect.t option Value.t
      -> ?on_deactivate:unit Effect.t option Value.t
      -> ?before_display:unit Effect.t option Value.t
      -> ?after_display:unit Effect.t option Value.t
      -> unit
      -> unit Computation.t

    (** [before_display] and [before_display'] are lower-level functions that can be used
        to register an event to occur once-per-frame (before the vdom is mounted) *)
    val before_display : here:[%call_pos] -> unit Effect.t Value.t -> unit Computation.t

    val before_display'
      :  here:[%call_pos]
      -> unit Effect.t option Value.t
      -> unit Computation.t

    (** [after_display] and [after_display'] are lower-level functions that can be used to
        register an event to occur once-per-frame (after each render). *)
    val after_display : here:[%call_pos] -> unit Effect.t Value.t -> unit Computation.t

    val after_display'
      :  here:[%call_pos]
      -> unit Effect.t option Value.t
      -> unit Computation.t

    (** [wait_after_display] is an effect that will complete after the next frame. *)
    val wait_after_display : here:[%call_pos] -> unit -> unit Effect.t Computation.t

    module Poll : sig
      module Starting : sig
        type ('o, 'r) t

        (** [empty] is an option to pass to the polling functions that changes its return
            type to be ['o option Computation.t] and starting value is [None] *)
        val empty : ('o, 'o option) t

        (** [initial x] is an option to pass to the polling functions that changes its
            return type to be ['o Computation.t] and the starting value is [x] *)
        val initial : 'o -> ('o, 'o) t
      end

      (** This function runs an effect every time that the input value changes, returning
          the most recent result as its computation.

          The [Starting.t] argument controls the type of the result, and depending on the
          value, will either return an optional value [Option.None] or a default value
          ['o] in the time in between the computation starting and the first result coming
          back from the effect. *)
      val effect_on_change
        :  here:[%call_pos]
        -> ?sexp_of_input:('a -> Sexp.t)
        -> ?sexp_of_result:('o -> Sexp.t)
        -> equal_input:('a -> 'a -> bool)
        -> ?equal_result:('o -> 'o -> bool)
        -> ('o, 'r) Starting.t
        -> 'a Value.t
        -> effect:('a -> 'o Effect.t) Value.t
        -> 'r Computation.t

      val manual_refresh
        :  here:[%call_pos]
        -> ?sexp_of_model:('o -> Sexp.t)
        -> ?equal:('o -> 'o -> bool)
        -> ('o, 'r) Starting.t
        -> effect:'o Effect.t Value.t
        -> ('r * unit Effect.t) Computation.t
    end
  end

  module Memo : sig
    (** The [Memo] module can be used to share a computation between multiple components,
        meaning that if the shared computation is stateful, then the users of that
        computation will see the same state.

        The way that [Memo] differs from just using [let%sub] on a computation and then
        passing the resulting [Value.t] down to its children is twofold:
        - The shared computation is not made active until it's actually requested by
          another component
        - Knowledge of any inputs to component are be deferred to "lookup time", when
          components request an instance of the component.

        Shared computations are refcounted, so when the last user of a memoized component
        deactivates, the shared component is deactivated as well. *)

    type ('input, 'result) t

    (** Creates a memo instance that can be used by calling [lookup] *)
    val create
      :  here:[%call_pos]
      -> ('input, 'cmp) Comparator.Module.t
      -> f:('input Value.t -> 'result Computation.t)
      -> ('input, 'result) t Computation.t

    (** Requests an instance of the shared computation for a given ['input] value. If an
        instance doesn't already exist, it will request a new computation, which results
        in [none] being returned for a brief period of time, after which it'll return a
        [Some] containing the result of that computation *)
    val lookup
      :  here:[%call_pos]
      -> ('input, 'result) t Value.t
      -> 'input Value.t
      -> 'result option Computation.t
  end

  module Effect_throttling : sig
    module Poll_result : sig
      type 'a t =
        | Aborted
        (** [Aborted] indicates that the effect was aborted before it even started. If an
            effect starts, then it should complete with some kind of result - [Effect]
            does not support cancellation in general. *)
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
        while a previous run is still ongoing will cause the new effect to be enqueued.
        Any previously enqueued item gets kicked out, thus maintaining the invariant that
        at most one effect will be enqueued. (this is important so that things like RPCs
        calls don't pile up)

        CAUTION: This computation assumes that the input effect will always complete. If a
        run of the effect raises, no more runs will ever get executed, since they will all
        be waiting for the one that raised to complete. *)
    val poll
      :  here:[%call_pos]
      -> ('a -> 'b Effect.t) Value.t
      -> ('a -> 'b Poll_result.t Effect.t) Computation.t
  end

  module Dynamic_scope : sig
    (** This module implements dynamic variable scoping. Once a dynamic variable is
        created, you can store values in it, and lookup those same values. A lookup will
        find the nearest-most parent "unreverted" [set] call where a "set" can be
        "reverted" with [set']'s [revert]. *)

    type 'a t

    (** Creates a new variable for use with the rest of the functions. It is critically
        important that the exact same [Dynamic_scope.t] is used in calls to [set] and the
        corresponding [lookup]. *)
    val create : ?sexp_of:('a -> Sexp.t) -> name:string -> fallback:'a -> unit -> 'a t

    (** Creates a variable which is derived from another. Typically this is used to
        project out a field of another dynamic variable which contains a record. *)
    val derived
      :  ?sexp_of:('a -> Sexp.t)
      -> 'b t
      -> get:('b -> 'a)
      -> set:('b -> 'a -> 'b)
      -> 'a t

    (** Given a ['a Dynamic_scope.t] and a ['a Value.t] evaluate a function whose
        resulting Computation.t has access to the value via the [lookup] function. *)
    val set
      :  here:[%call_pos]
      -> 'a t
      -> 'a Value.t
      -> inside:'r Computation.t
      -> 'r Computation.t

    type revert = { revert : 'a. 'a Computation.t -> 'a Computation.t }

    (** like [set] but with the ability to revert the value in sub-computations. *)
    val set'
      :  here:[%call_pos]
      -> 'a t
      -> 'a Value.t
      -> f:(revert -> 'r Computation.t)
      -> 'r Computation.t

    (** Lookup attempts to find the value inside the nearest scope, but if there isn't
        one, it falls back to default specified in [create]. *)
    val lookup : here:[%call_pos] -> 'a t -> 'a Computation.t

    val modify
      :  here:[%call_pos]
      -> 'a t
      -> change:('a Value.t -> 'a Value.t)
      -> f:(revert -> 'r Computation.t)
      -> 'r Computation.t
  end

  module Incr : sig
    (** A [Value.t] passed through [value_cutoff] will only trigger changes on its
        dependents when the value changes according to the provided equality function *)
    val value_cutoff
      :  here:[%call_pos]
      -> 'a Value.t
      -> equal:('a -> 'a -> bool)
      -> 'a Computation.t

    (** Use [compute] to move a function from the incremental world into the bonsai world. *)
    val compute
      :  here:[%call_pos]
      -> 'a Value.t
      -> f:('a Incr.t -> 'b Incr.t)
      -> 'b Computation.t

    (** If you've got an incremental, you can convert it to a value with this function. *)
    val to_value : here:[%call_pos] -> 'a Incr.t -> 'a Value.t

    (** Compute some incremental value based on the time source. Using this time source
        instead of [Incr.clock] is the more testable approach, since it allows tests to
        control how time moves forward. *)
    val with_clock : here:[%call_pos] -> (Time_source.t -> 'a Incr.t) -> 'a Computation.t
  end

  (** This [Let_syntax] module is basically just {!Value.Let_syntax} with the addition of
      the [sub] function, which operates on Computations.

      By using the [let%sub] syntax extension, you can put a ['a Computation.t] on the RHS
      and get a ['a Value.t] on the LHS.

      {[
        let%sub a = b in
        ...
      ]}

      In the code above, [b] has type ['a Computation.t], and [a] has type ['a Value.t]. *)
  module Let_syntax : sig
    (*_ [let%pattern_bind] requires that a function named [return] with these semantics
        exist here. *)
    val return : here:[%call_pos] -> 'a Value.t -> 'a Computation.t
    val ( >>| ) : here:[%call_pos] -> 'a Value.t -> ('a -> 'b) -> 'b Value.t
    val ( <*> ) : here:[%call_pos] -> ('a -> 'b) Value.t -> 'a Value.t -> 'b Value.t
    val ( <$> ) : here:[%call_pos] -> ('a -> 'b) -> 'a Value.t -> 'b Value.t

    module Let_syntax : sig
      (** [sub] instantiates a computation and provides a reference to its results to [f]
          in the form of a [Value.t]. The main way to use this function is via the
          [let%sub] syntax extension. [here:[%call_pos]] is used by the Bonsai debugger to
          tie visualizations to precise source locations. *)
      val sub
        :  here:[%call_pos]
        -> 'a Computation.t
        -> f:('a Value.t -> 'b Computation.t)
        -> 'b Computation.t

      val cutoff
        :  here:[%call_pos]
        -> 'a Value.t
        -> equal:('a -> 'a -> bool)
        -> 'a Value.t

      val switch
        :  here:Source_code_position.t
        -> match_:int Value.t
        -> branches:int
        -> with_:(int -> 'a Computation.t)
        -> 'a Computation.t

      val map : here:[%call_pos] -> 'a Value.t -> f:('a -> 'b) -> 'b Value.t

      val map2
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> f:('a -> 'b -> 'c)
        -> 'c Value.t

      val return : here:[%call_pos] -> 'a Value.t -> 'a Computation.t
      val both : here:[%call_pos] -> 'a Value.t -> 'b Value.t -> ('a * 'b) Value.t
      val arr : here:[%call_pos] -> 'a Value.t -> f:('a -> 'b) -> 'b Computation.t

      include Mapn with type 'a t := 'a Value.t

      val arr2
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> f:('a -> 'b -> 'c)
        -> 'c Computation.t

      val arr3
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> 'c Value.t
        -> f:('a -> 'b -> 'c -> 'd)
        -> 'd Computation.t

      val arr4
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> 'c Value.t
        -> 'd Value.t
        -> f:('a -> 'b -> 'c -> 'd -> 'e)
        -> 'e Computation.t

      val arr5
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> 'c Value.t
        -> 'd Value.t
        -> 'e Value.t
        -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f)
        -> 'f Computation.t

      val arr6
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> 'c Value.t
        -> 'd Value.t
        -> 'e Value.t
        -> 'f Value.t
        -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
        -> 'g Computation.t

      val arr7
        :  here:[%call_pos]
        -> 'a Value.t
        -> 'b Value.t
        -> 'c Value.t
        -> 'd Value.t
        -> 'e Value.t
        -> 'f Value.t
        -> 'g Value.t
        -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h)
        -> 'h Computation.t
    end
  end

  module Time_source = Time_source

  module Expert : sig
    (** [thunk] will execute its argument exactly once per instantiation of the
        computation. *)
    val thunk : here:[%call_pos] -> (unit -> 'a) -> 'a Computation.t

    (** [assoc_on] is similar to [assoc], but allows the model to be keyed differently
        than the input map. This comes with a few caveats:

        - Inputs whose keys map to the same [model_key] will share the same model.
        - The result of [get_model_key] is used in a bind, so it is expensive when it
          changes.

        [assoc] should almost always be used instead. Consider whether you really need the
        additional power before reaching for this function. *)
    val assoc_on
      :  here:[%call_pos]
      -> ('io_key, 'io_cmp) Comparator.Module.t
      -> ('model_key, 'model_cmp) Comparator.Module.t
      -> ('io_key, 'data, 'io_cmp) Map.t Value.t
      -> get_model_key:('io_key -> 'data -> 'model_key)
      -> f:('io_key Value.t -> 'data Value.t -> 'result Computation.t)
      -> ('io_key, 'result, 'io_cmp) Map.t Computation.t
  end

  module Debug : sig
    (** [on_change v ~f] executes the function [f] every time that [v] is recomputed. *)
    val on_change : here:[%call_pos] -> 'a Value.t -> f:('a -> unit) -> unit Computation.t

    (** like [on_change], but specialized for printing a sexp of the value that you are
        watching. *)
    val on_change_print_s
      :  here:[%call_pos]
      -> 'a Value.t
      -> ('a -> Sexp.t)
      -> unit Computation.t

    val to_dot : ?pre_process:bool -> 'a Computation.t -> string

    val bonsai_node_counts
      :  ?pre_process:bool
      -> 'a Computation.t
      -> Bonsai.Private.Skeleton.Counts.t

    val enable_incremental_annotations : unit -> unit
    val disable_incremental_annotations : unit -> unit
  end

  val path : here:[%call_pos] -> unit -> Path.t Computation.t

  (** Analog to [Incr_map] functions in Bonsai. In general, you should prefer to use
      [Bonsai.assoc] where possible. For functions that are particularly easy to implement
      in terms of [assoc], the function is stubbed with a [ `Use_assoc ] value instead. We
      also skip wrapping the prime versions of [Incr_map] functions, since they more
      easily allow [Incr.bind], which we want to make sure is used only when absolutely
      necessary. *)
  include
    Bonsai.Private.For_proc.Map_and_set0_output
    with module Value := Value
     and module Computation := Computation
end
