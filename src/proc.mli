open! Core_kernel
open! Import

type 'a private_value := 'a Value.t
type 'a private_computation := 'a Computation.packed

(** The functions found in this module are focused on the manipulation
    of values of type ['a Computation.t] and ['a Value.t].  There are fine
    descriptions of these types below and how to use them, but since it's
    so common to convert between the two, here is a cheat-sheet matrix for
    converting between values of different types:

    {v

    | Have \ Want      | 'a Value.t             | 'a Computation.t |
    |------------------+------------------------+------------------|
    | 'a               | let v = Value.return a | let c = const a  |
    | 'a Value.t       |                        | let c = read v   |
    | 'a Computation.t | let%sub v = c          |                  |

    v}
*)

module Computation : sig
  (** A value of type ['a Computation.t] represents a computation which produces a value
      that may change during the lifetime of a program, and the value may be influenced by
      the internal state of that computation.

      The same ['a Computation.t] can be used in multiple places in a program, and these
      uses will {e not} share the same state, nor will they share the work performed by
      the computation.

      In this normal OCaml code, if we see the same function being called multiple times:

      {[
        let a = f () in
        let b = f () in
        a + b
      ]}

      You would not be surprised to know that if [f] has side-effects (maybe
      printing to the console), then those side-effects happen twice because
      [f] was called twice.

      Similarly, if we wrote the code this way:

      {[
        let a = f () in
        let b = a in
        a + b
      ]}

      You would (correctly) expect that the side-effect only happens once, when computing
      [a].  In these examples, the {e code} [f ()] is analogous to [_ Computation.t].  If
      you want to have two separate values whose computations maintain separate state, you
      would use two instances of "let%sub" to bind them separately:

      {[
        val some_computation : int Computation.t
        val add : int Value.t -> int Value.t -> int Computation.t

        let open Proc.Let_syntax in
        let%sub a = some_computation in
        let%sub b = some_computation in
        add a b
      ]}

      Here, [a] and [b] can take on different values depending on the states of the
      computations that produce them.

      However, if you want to use just one value in multiple places, only use
      [let%sub] once:

      {[
        let open Proc.Let_syntax in
        let%sub a = some_computation in
        let     b = a in
        add a b
      ]}

      Here, [a] and [b] always take on the same value.
  *)
  type 'a t
end

module Value : sig
  (** A value of type ['a Value.t] represents a value that may change during the lifetime
      of the program.  For those familiar with the [Incremental] library, this type is
      conceptually very similar to [Incr.t].  The main method by which you acquire values
      of type [Value.t] is by using the [let%sub] syntax extension.

      {[
        val c : int Computation.t

        let%sub x = c in
        (* [x] has type [int Value.t] here *)
      ]}

      In the example above, we run a computation [c] and store the result of that
      computation in [x] which has type [Value.t].

      [Value.t] is an applicative, which means that you can combine multiple [Value]s into
      one by using [Proc.Let_syntax]:

      {[
        val a : int Value.t
        val b : int Value.t

        let open Proc.Let_syntax in
        let%map a = a and b = b in
        a + b
      ]}
  *)
  type 'a t

  include Applicative.S with type 'a t := 'a t
  include Mapn with type 'a t := 'a t

  (** A [Value.t] transformed by [cutoff] will only trigger changes on its dependents when the equality
      of the contained value has changed. *)
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
end

module Var : sig
  (** A [Var.t] is the primary method for making data obtained outside of Bonsai (maybe via
      an RPC) accessible inside a Bonsai application. *)
  type 'a t

  (** Creates a new [Var.t] with an initial value. *)
  val create : 'a -> 'a t

  (** Updates the value inside of [t].  [f] is given the previous value of [t] so that you
      can reuse parts of the value if applicable *)
  val update : 'a t -> f:('a -> 'a) -> unit

  (** Sets the value inside of [t]. *)
  val set : 'a t -> 'a -> unit

  (** Provides read-only access to [t] by producing a {!Value.t} which is used inside of a
      Bonsai computation. *)
  val value : 'a t -> 'a Value.t
end

(** Converts a [Value.t] to a [Computation.t].  Unlike most Computations, the [Computation.t]
    returned by [read] can be used in multiple locations without maintaining multiple copies of
    any models or building duplicate incremental graphs.

    [read] is most commonly used in the final expression of a [let%sub] chain, like so:

    {[
      fun i ->
        let%sub a = f i in
        let%sub b = g i in
        read
          (let%map a = a
           and b = b in
           a + b)
    ]}

    or to use some APIs that require [Computation.t] like so:

    {[
      val cond : bool Value.t
      val x : 'a Value.t
      val some_computation : 'a Computation.t

      let y = if_ cond ~then_:some_computation ~else_:(read x)
      val y : 'a Computation.t
    ]}
*)
val read : 'a Value.t -> 'a Computation.t

(** Creates a [Computation.t] that provides a constant value. *)
val const : 'a -> 'a Computation.t

(** Lifts a regular OCaml function into one that takes a Value as input, and produces
    a Computation as output. *)
val pure : ('a -> 'b) -> 'a Value.t -> 'b Computation.t

(** Given a first-class module that has no input (unit input type), and the default
    value of the state machine, [of_module0] will create a [Computation] that produces
    values of that module's [Result.t] type. *)
val of_module0 : (unit, 'm, 'a, 'r) component_s -> default_model:'m -> 'r Computation.t

(** The same as {!of_module0}, but this one has an input type ['i].  Because input to the
    component is required, this function also expects a [Value.t] that provides its input.
    It is common for this function to be partially applied like so:

    {[
      val a : int Value.t
      val b : int Value.t

      let f = of_module1 (module struct ... end) ~default_model in
      let%sub a = f a in
      let%sub b = f b in
      ...
    ]}

    Where the [Value.t] values are passed in later. *)
val of_module1
  :  ('i, 'm, 'a, 'r) component_s
  -> default_model:'m
  -> 'i Value.t
  -> 'r Computation.t

(** The same as {!of_module1} but with two inputs. *)
val of_module2
  :  ('i1 * 'i2, 'm, 'a, 'r) component_s
  -> default_model:'m
  -> 'i1 Value.t
  -> 'i2 Value.t
  -> 'r Computation.t

(** A constructor for [Computation.t] that models a simple state machine.
    The first-class module implementing [Model] describes the states in
    the state machine, while the first-class module implementing [Action]
    describes the transitions between states.

    [default_model] is the initial state for the state machine, and [apply_action]
    implements the transition function that looks at the current state and the requested
    transition, and produces a new state.

    (It is very common for [inject] and [schedule_event] to be unused) *)
val state_machine0
  :  Source_code_position.t
  -> (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> default_model:'model
  -> apply_action:
       (inject:('action -> Event.t)
        -> schedule_event:(Event.t -> unit)
        -> 'model
        -> 'action
        -> 'model)
  -> ('model * ('action -> Event.t)) Computation.t

(** A frequently used state-machine is the trivial 'set-state' transition,
    where the action always replaces the value contained inside.  This
    helper-function implements that state-machine, providing access to the
    current state, as well as an inject function that updates the state. *)
val state
  :  Source_code_position.t
  -> (module Model with type t = 'model)
  -> default_model:'model
  -> ('model * ('model -> Event.t)) Computation.t

(** Similar to [state], but stores an option of the model instead.
    [default_model] is optional and defaults to [None].  *)
val state_opt
  :  Source_code_position.t
  -> ?default_model:'model
  -> (module Model with type t = 'model)
  -> ('model option * ('model option -> Event.t)) Computation.t

(** The same as {!state_machine0}, but [apply_action] also takes an input from a
    [Value.t]. *)
val state_machine1
  :  Source_code_position.t
  -> (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> default_model:'model
  -> apply_action:
       (inject:('action -> Event.t)
        -> schedule_event:(Event.t -> unit)
        -> 'input
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Value.t
  -> ('model * ('action -> Event.t)) Computation.t

(** Because all Bonsai computation-returning-functions are eagerly evaluated, attempting
    to use "let rec" to construct a recursive component will recurse infinitely.  One way
    to avoid this is to use a lazy computation and [Bonsai.lazy_] to defer evaluating the
    [Computation.t].

    {[
      let rec some_component arg1 arg2 =
        ...
        let _ = Bonsai.lazy_ (lazy (some_component ...)) in
        ...
    ]} *)
val lazy_ : 'a Computation.t Lazy.t -> 'a Computation.t

(** [assoc] is used to apply a Bonsai computation to each element of a map.  This function
    signature is very similar to [Map.mapi] or [Incr_map.mapi'], and for good reason!

    It is doing the same thing (taking a map and a function and returning a new map with
    the function applied to every key-value pair), but this function does it with the
    Bonsai values, which means that the computation is done incrementally and also
    maintains a state machine for every key-value pair. *)
val assoc
  :  ('key, 'cmp) comparator
  -> ('key, 'data, 'cmp) Map.t Value.t
  -> f:('key Value.t -> 'data Value.t -> 'result Computation.t)
  -> ('key, 'result, 'cmp) Map.t Computation.t


(** [enum] is used for matching on a value and providing different behaviors on different
    values.  The type of the value must be enumerable (there must be a finite number of
    possible values), and it must be comparable and sexpable.

    The rest of the parameters are named like you might expect from pattern-matching
    syntax, with [match_] taking the value to match on, and [with_] taking a function that
    choose which behavior to use. *)
val enum
  :  (module Enum with type t = 'k)
  -> match_:'k Value.t
  -> with_:('k -> 'a Computation.t)
  -> 'a Computation.t

(** [if_] is a special case of {!enum} for bools. *)
val if_
  :  bool Value.t
  -> then_:'a Computation.t
  -> else_:'a Computation.t
  -> 'a Computation.t

(** [wrap] wraps a Computation (built using [f]) and provides a model and
    injection function that the wrapped component can use.  Especially of note
    is that the [apply_action] for this outer-model has access to the result
    value of the Computation being wrapped. *)
val wrap
  :  (module Model with type t = 'model)
  -> default_model:'model
  -> apply_action:
       (inject:('action -> Event.t)
        -> schedule_event:(Event.t -> unit)
        -> 'result
        -> 'model
        -> 'action
        -> 'model)
  -> f:('model Value.t -> ('action -> Event.t) Value.t -> 'result Computation.t)
  -> 'result Computation.t

(** [match_either] enables matching on a value of type [('a, 'b) Either.t]
    via the [first] and [second] arguments. *)
val match_either
  :  ('a, 'b) Either.t Value.t
  -> first:('a Value.t -> 'c Computation.t)
  -> second:('b Value.t -> 'c Computation.t)
  -> 'c Computation.t

(** [match_result] enables matching on a value of type [('a, 'b) Result.t]
    via the [ok] and [err] arguments. *)
val match_result
  :  ('a, 'b) Result.t Value.t
  -> ok:('a Value.t -> 'c Computation.t)
  -> err:('b Value.t -> 'c Computation.t)
  -> 'c Computation.t

(** [map_option] is similar to [Option.map] but with Bonsai's types instead.
    The computation produced by [f] will only be run when the input option is
    [some]. *)
val map_option
  :  'a option Value.t
  -> f:('a Value.t -> 'b Computation.t)
  -> 'b option Computation.t

(** [match_option] enables matching on a value of type ['a option]. *)
val match_option
  :  'a option Value.t
  -> some:('a Value.t -> 'b Computation.t)
  -> none:'b Computation.t
  -> 'b Computation.t

(** [with_model_resetter] extends a computation with the ability to reset the
    state machine for that computation back to its default.  This can be useful
    for e.g. clearing a form of all input values.*)
val with_model_resetter : 'a Computation.t -> ('a * Event.t) Computation.t

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
  val return : 'a Value.t -> 'a Computation.t
  val ( >>| ) : 'a Value.t -> ('a -> 'b) -> 'b Value.t
  val ( <*> ) : ('a -> 'b) Value.t -> 'a Value.t -> 'b Value.t
  val ( <$> ) : ('a -> 'b) -> 'a Value.t -> 'b Value.t

  module Let_syntax : sig
    (** [sub] runs a Computation, providing the result of that Computation to the
        function [f] in the form of a [Value.t].  The main way to use this function is via
        the syntax extension [let%sub] which is described above. *)
    val sub : 'a Computation.t -> f:('a Value.t -> 'b Computation.t) -> 'b Computation.t

    val map : 'a Value.t -> f:('a -> 'b) -> 'b Value.t
    val return : 'a -> 'a Value.t
    val both : 'a Value.t -> 'b Value.t -> ('a * 'b) Value.t

    include Mapn with type 'a t := 'a Value.t
  end
end

module Private : sig
  val reveal_value : 'a Value.t -> 'a private_value
  val conceal_value : 'a private_value -> 'a Value.t
  val reveal_computation : 'a Computation.t -> 'a private_computation
  val conceal_computation : 'a private_computation -> 'a Computation.t
end
