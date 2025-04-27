open! Core
open! Import

(** New apps should use the [Bonsai] library instead. *)
type ('input, 'result) t = 'input Bonsai_proc.Value.t -> 'result Bonsai_proc.Computation.t

(** [const] creates a Bonsai component with an unchanging output. It does not have an
    input, or a model. Constant components are not frequently used, but can be handy when
    using an API that requires a [Bonsai.t], and you want the result to be constant.

    This function is approximately the same as [Fn.const]. *)
val const : 'result -> (_, 'result) t

(** [input] is the identity function as a Bonsai component. *)
val input : ('result, 'result) t

(** [pure] is used to create a Bonsai component that can be implemented as a pure function
    from ['input] to ['result] *)
val pure : f:('input -> 'result) -> ('input, 'result) t

(** [compose] (and the [>>>] infix operator) connect the output from one component into
    the input of another component. This is conceptually similar to function composition:
    [Fn.compose]. *)
val compose : ('input, 's) t -> ('s, 'result) t -> ('input, 'result) t

(** [map] (and the [>>|] infix operator ) transforms the result type of a Bonsai component
    using the provided function. *)
val map : ('input, 'r1) t -> f:('r1 -> 'r2) -> ('input, 'r2) t

(** [map_input] (and the [@>>] infix operator) transforms the input type of a Bonsai
    component using the provided function. *)
val map_input : ('i2, 'result) t -> f:('i1 -> 'i2) -> ('i1, 'result) t

(** [of_module] is one of the most commonly used component constructors. The function
    takes a first-class module of type [Component_s], as well as the default model for the
    component. For more details, please read the docs for [Component_s]. *)
val of_module
  :  here:[%call_pos]
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ('input, 'model, 'action, 'result) component_s
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> ('input, 'result) t

(** Given two components that have the same input, [both] returns a Bonsai component that
    contains both of their outputs. *)
val both : here:[%call_pos] -> ('input, 'r1) t -> ('input, 'r2) t -> ('input, 'r1 * 'r2) t

(** [state_machine] is a function that is used to define a component solely in terms of
    its [apply_action] function. The result value of the component is the value of the
    current model alongside "context" that includes an injection function to transition
    the state machine *)
val state_machine
  :  sexp_of_action:('action -> Sexp.t)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> equal:('model -> 'model -> bool)
  -> Source_code_position.t
  -> default_model:'model
  -> apply_action:
       (('action, unit) Apply_action_context.t -> 'input -> 'model -> 'action -> 'model)
  -> ('input, 'model * ('action -> unit Effect.t)) t

(** [enum] is how a Bonsai component can branch on its input and handle different cases
    with a different Bonsai component.

    The [which] function translates cases from the components input into values of type
    ['key].

    The [handle] function translates the values returned by [which] into the component
    that handles this value. *)
val enum
  :  here:[%call_pos]
  -> (module Enum with type t = 'key)
  -> which:('input -> 'key)
  -> handle:('key -> ('input, 'result) t)
  -> ('input, 'result) t

(** [if_] is a special case of [enum] for booleans. *)
val if_
  :  here:[%call_pos]
  -> ('input -> bool)
  -> then_:('input, 'result) t
  -> else_:('input, 'result) t
  -> ('input, 'result) t

module type S = Module_types.Component_s

module Map : sig
  (** [assoc_input] is used as a mapping function on a [Map.t] in order to produce a new
      map. *)
  val assoc_input
    :  ('key, 'cmp) Comparator.Module.t
    -> ('data, 'result) t
    -> (('key, 'data, 'cmp) Map.t, ('key, 'result, 'cmp) Map.t) t

  (** The same as [assoc_input] but the component has access to the key as well as the
      data. *)
  val associ_input
    :  ('key, 'cmp) Comparator.Module.t
    -> ('key * 'data, 'result) t
    -> (('key, 'data, 'cmp) Map.t, ('key, 'result, 'cmp) Map.t) t

  (** The same as [associ_input] but with some extra stuff to be provided to the component
      in addition to the key and data. *)
  val associ_input_with_extra
    :  ('key, 'cmp) Comparator.Module.t
    -> ('key * 'data * 'input, 'result) t
    -> (('key, 'data, 'cmp) Map.t * 'input, ('key, 'result, 'cmp) Map.t) t
end

(** [arr] is the same as [pure]. *)
val arr : ('a -> 'b) -> ('a, 'b) t

(** The same as [map] *)
val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t

(** The same as [map_input] *)
val ( ^>> ) : ('a, 'b) t -> ('c -> 'a) -> ('c, 'b) t

(** [first t] applies [t] to the first part of the input. *)
val first : here:[%call_pos] -> ('input, 'result) t -> ('input * 'a, 'result * 'a) t

(** [second t] applies [t] to the second part of the input. *)
val second : here:[%call_pos] -> ('input, 'result) t -> ('a * 'input, 'a * 'result) t

(** [split t u] applies [t] to the first part of the input and [u] to the second part. *)
val split : here:[%call_pos] -> ('i1, 'r1) t -> ('i2, 'r2) t -> ('i1 * 'i2, 'r1 * 'r2) t

(** [extend_first] returns the result of a Bonsai component alongside its input. *)
val extend_first : here:[%call_pos] -> ('input, 'result) t -> ('input, 'result * 'input) t

(** [extend_second] returns the result of a Bonsai component alongside its input. *)
val extend_second
  :  here:[%call_pos]
  -> ('input, 'result) t
  -> ('input, 'input * 'result) t

(** [fanout t u] applies [t] and [u] to the same input and returns both results. It's
    actually just [both]. *)
val fanout
  :  here:[%call_pos]
  -> ('input, 'r1) t
  -> ('input, 'r2) t
  -> ('input, 'r1 * 'r2) t

(** [t *** u = split t u]. *)
val ( *** ) : here:[%call_pos] -> ('i1, 'r1) t -> ('i2, 'r2) t -> ('i1 * 'i2, 'r1 * 'r2) t

(** [t &&& u = fanout t u]. *)
val ( &&& )
  :  here:[%call_pos]
  -> ('input, 'r1) t
  -> ('input, 'r2) t
  -> ('input, 'r1 * 'r2) t

(** Composes two components where one of the outputs of the first component is one of the
    inputs to the second. *)
val partial_compose_first
  :  here:[%call_pos]
  -> ('input, 'shared * 'output1) t
  -> ('input * 'shared, 'output2) t
  -> ('input, 'output1 * 'output2) t

(** [pipe] connects two components, but provides several functions that ease the
    transference of data between the components, as well as collect the final result. *)
val pipe
  :  here:[%call_pos]
  -> ('input, 'r1) t
  -> into:('intermediate, 'r2) t
  -> via:('input -> 'r1 -> 'intermediate)
  -> finalize:('input -> 'r1 -> 'r2 -> 'r3)
  -> ('input, 'r3) t

module With_incr : sig
  (** Constructs a Bonsai component whose result is always the same as its input
      Incremental node. *)
  val of_incr : 'a Incr.t -> (_, 'a) t

  val of_module
    :  here:[%call_pos]
    -> ?sexp_of_model:('m -> Sexp.t)
    -> ('i, 'm, 'a, 'r) component_s_incr
    -> equal:('m -> 'm -> bool)
    -> default_model:'m
    -> ('i, 'r) t

  (** Transforms the result of a component, exposing the incrementality for optimization
      purposes. *)
  val map : ('i, 'r1) t -> f:('r1 Incr.t -> 'r2 Incr.t) -> ('i, 'r2) t

  (** Same as [Bonsai.pure] but allows the user to optimize using Incremental. *)
  val pure : f:('i Incr.t -> 'r Incr.t) -> ('i, 'r) t

  (** Creates a Bonsai component where the given cutoff is applied to the incremental node
      as input passes through the component, preventing a component from being
      recalculated unnecessarily.

      See [Incr.set_cutoff]. *)
  val value_cutoff : cutoff:'i Incr.Cutoff.t -> ('i, 'i) t
end

module Infix : sig
  (** [a >>> b] is [compose a b] *)
  val ( >>> ) : ('i1, 'r1) t -> ('r1, 'r2) t -> ('i1, 'r2) t

  (** [a >>| f] is [map a ~f] *)
  val ( >>| ) : ('input, 'r1) t -> ('r1 -> 'r2) -> ('input, 'r2) t

  (** [f @>> a] is [map_input a ~f] *)
  val ( @>> ) : ('i1 -> 'i2) -> ('i2, 'result) t -> ('i1, 'result) t
end

module Let_syntax : sig
  val return : 'result -> (_, 'result) t

  include module type of Infix

  module Let_syntax : sig
    val return : 'result -> (_, 'result) t
    val map : ('input, 'r1) t -> f:('r1 -> 'r2) -> ('input, 'r2) t

    val both
      :  here:[%call_pos]
      -> ('input, 'r1) t
      -> ('input, 'r2) t
      -> ('input, 'r1 * 'r2) t

    module Open_on_rhs : module type of Infix
  end
end
