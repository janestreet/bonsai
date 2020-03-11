open! Core_kernel
open! Import


module type S_gen = sig
  module Incr : Incremental.S
  module Event : Event.S


  (** Default is [`Ignore]. *)
  type ('extra, 'new_model) on_action_mismatch =
    [ `Ignore
    | `Raise
    | `Warn
    | `Custom of 'extra -> 'new_model
    ]

  (** The component type ([('input, 'model, 'result) Bonsai.t]) can be thought of as a
      function from ['model] to ['result], but where the ['result] can schedule events of
      the component's "action type".  These actions are used to produce a new ['model]
      which in turn causes the ['result] to be recomputed. Instances of the ['result] type
      can contain callbacks which schedule actions when interacted with by user (via
      button click, text input, etc). These actions are handled by the component's
      [apply_action] function, which yields a new model to be displayed. *)
  type ('input, 'model, 'result) t

  val sexp_of_t : (_, _, _) t -> Sexp.t

  (** A bonsai component that just forwards the input straight through to the result.
      This is equivalent to [Bonsai.pure ~f:Fn.id]. *)
  val input : ('input, _, 'input) t

  (** A bonsai component that forwards the model into the result.
      This is equivalent to [Bonsai.pure_of_model ~f:Fn.id]. *)
  val model : (_, 'model, 'model) t

  (** {1 Component Constructors} *)

  (** Returns a component with no action or model, only a constant result. *)
  val const : 'result -> (_, _, 'result) t

  (** A pure function with no model from 'input to 'result *)
  val pure : f:('input -> 'result) -> ('input, _, 'result) t

  (** Like [pure], but gets the input from the ['model]. *)
  val pure_from_model : f:('model -> 'result) -> (_, 'model, 'result) t

  (** [compose a b] joins [a] and [b] together such that the result of [a] is used as the
      input of [b]. *)
  val compose : ('i1, 'model, 'r1) t -> ('r1, 'model, 'r2) t -> ('i1, 'model, 'r2) t

  (** Many modules have the same shape, they declare the model, action, and result of the
      component, and then define apply_action and view over those types.

      This is intended to be used with the {!of_module} function. *)
  module type S = sig
    (** A component receives read-only input, either as output from other
        components or from an external system (e.g. data from a server).  The input
        is frequently dynamic, but may also be constant. *)
    module Input : sig
      type t
    end

    (** A component's model is a state-machine that the component can read, but
        also write to.  Because both the input and model are readable, it can be hard
        to decide whether to request some data from the input or the model.  It is
        highly recommended to put just the data that needs mutation in [Model.t], and
        the rest in [Input.t]. *)
    module Model : sig
      type t
    end

    (** Components can change their own [Model.t] by issuing "actions" that
        perform the state transition.  If you think of the state machine as
        having state-nodes of type [Model.t], then the arrows between those nodes
        would be of type [Action.t]. *)
    module Action : sig
      type t [@@deriving sexp_of]
    end

    (** While UI components stereotypically produce some kind of "view", with
        Bonsai, components are small and easy enough to compose that Bonsai
        components frequently produce intermediate results which are then wired into
        other components. *)
    module Result : sig
      type t
    end

    (** When an action is raised by this component (via an Event.t), Bonsai
        will eventually pass that action back to that component's [apply_action]
        function.  This function is responsible for looking at the model and the
        incoming action and producing a new model.

        [apply_action] is a transformation from a model and an action into a new model.
        During the transformation, the component can also emit more actions via
        [schedule_event] or use Async to arrange for [schedule_event] to be called later.
    *)
    val apply_action
      :  inject:(Action.t -> Event.t)
      -> schedule_event:(Event.t -> unit)
      -> Input.t
      -> Model.t
      -> Action.t
      -> Model.t

    (** [compute] is a function from input and model to the component's result.
        In a component that produces a view, this function could be thought of as the
        "view computation function".

        This function is also given an "inject" function which converts this
        component's [Action.t] to a global [Event.t] which can be given to
        Bonsai to schedule.  Frequently, this Event.t is embedded within the
        result as a handler for some kind of user input. *)
    val compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t

    (** The name of the component.  This is used to identify the component while
        debugging, and to annotate error messages.  If you can't think of a good
        name, a reasonable fallback is [Source_code_position.to_string [%here]]. *)
    val name : string
  end

  type ('input, 'model, 'action, 'result) component_s =
    (module S
      with type Input.t = 'input
       and type Model.t = 'model
       and type Action.t = 'action
       and type Result.t = 'result)

  module M (Component : S) : sig
    type nonrec t = (Component.Input.t, Component.Model.t, Component.Result.t) t
  end

  val of_module
    :  ('input, 'model, 'action, 'result) component_s
    -> ('input, 'model, 'result) t

  module type Enum = sig
    type t [@@deriving sexp_of, compare, enumerate]
  end

  val enum
    :  ?on_action_mismatch:('key * 'model, 'model) on_action_mismatch
    -> (module Enum with type t = 'key)
    -> which:('input -> 'model -> 'key)
    -> handle:('key -> ('input, 'model, 'result) t)
    -> ('input, 'model, 'result) t

  (** [if_] is a simple application of [enum] to [(module Bool)]. *)
  val if_
    :  ?on_action_mismatch:(bool * 'model, 'model) on_action_mismatch
    -> ('input -> 'model -> bool)
    -> then_:('input, 'model, 'result) t
    -> else_:('input, 'model, 'result) t
    -> ('input, 'model, 'result) t

  module Infix : sig
    (** [a >>> b] is [compose a b] *)
    val ( >>> ) : ('i1, 'model, 'r1) t -> ('r1, 'model, 'r2) t -> ('i1, 'model, 'r2) t

    (** [t >>| f] is [map t ~f] *)
    val ( >>| ) : ('input, 'model, 'r1) t -> ('r1 -> 'r2) -> ('input, 'model, 'r2) t

    (** [f @>> t] is the [map_input t ~f] *)
    val ( @>> ) : ('i1 -> 'i2) -> ('i2, 'model, 'result) t -> ('i1, 'model, 'result) t
  end

  (** For composing components which share the same model.  For example, applying an
      action in one component changes the shared model, which is reflected in the results
      of the other component. *)
  include
    Applicative.S3 with type ('r, 'i, 'm) t := ('i, 'm, 'r) t

  (** Transforms the input of a component. The signature of [f] is reversed from most
      other map functions. *)
  val map_input : ('i2, 'model, 'result) t -> f:('i1 -> 'i2) -> ('i1, 'model, 'result) t

  module Let_syntax : sig
    val return : 'result -> (_, _, 'result) t

    include module type of Infix

    module Let_syntax : sig
      val return : 'result -> (_, _, 'result) t
      val map : ('input, 'model, 'r1) t -> f:('r1 -> 'r2) -> ('input, 'model, 'r2) t

      val both
        :  ('input, 'model, 'r1) t
        -> ('input, 'model, 'r2) t
        -> ('input, 'model, 'r1 * 'r2) t

      module Open_on_rhs : module type of Infix
    end
  end

  (** {1 Combinators} *)

  module Model : sig
    val f
      :  ('input, 'm1, 'result) t
      -> get:('m2 -> 'm1)
      -> set:('m2 -> 'm1 -> 'm2)
      -> ('input, 'm2, 'result) t

    val ignore : ('input, unit, 'result) t -> ('input, 'model, 'result) t

    val field
      :  ('model_outer, 'model_inner) Field.t
      -> ('input, 'model_inner, 'result) t
      -> ('input, 'model_outer, 'result) t

    val state_machine
      :  Source_code_position.t
      -> sexp_of_action:('action -> Sexp.t)
      -> apply_action:(inject:('action -> Event.t)
                       -> schedule_event:(Event.t -> unit)
                       -> 'input
                       -> 'model
                       -> 'action
                       -> 'model)
      -> ('input, 'model, 'action -> Event.t) t

    (** Takes a component that operates immutably over an input (via 'input), and
        converts it to a component that gets its input from the model. *)
    val to_input : ('input, unit, 'result) t -> (unit, 'input, 'result) t

    (** Takes a component that operates immutably over an input (via 'input), and
        converts it to a component that gets part of that input from the model. *)
    val to_input_with_other
      :  ('input * 'model, unit, 'result) t
      -> ('input, 'model, 'result) t
  end

  module Map : sig
    (** Transforms a component into a new component whose model and result are maps. *)
    val assoc_model
      :  ?comparator:('k, 'cmp) Core_kernel.Map.comparator
      -> ('input, 'model, 'result) t
      -> ( 'input
         , ('k, 'model, 'cmp) Core_kernel.Map.t
         , ('k, 'result, 'cmp) Core_kernel.Map.t )
           t

    (** Transforms a component into a new component whose model and result are maps.  The
        input for the transformed component also receives its key as an input. *)
    val associ_model
      :  ?comparator:('k, 'cmp) Core_kernel.Map.comparator
      -> ('k * 'input, 'model, 'result) t
      -> ( 'input
         , ('k, 'model, 'cmp) Core_kernel.Map.t
         , ('k, 'result, 'cmp) Core_kernel.Map.t )
           t

    (** Transforms a component into a new component whose input and result are maps. *)
    val assoc_input
      :  ?comparator:('k, 'cmp) Core_kernel.Map.comparator
      -> ('input, 'model, 'result) t
      -> ( ('k, 'input, 'cmp) Core_kernel.Map.t
         , 'model
         , ('k, 'result, 'cmp) Core_kernel.Map.t )
           t

    (** Transforms a component into a new component whose input and result are maps.
        The input for the transformed component also receives its key as an input.  *)
    val associ_input
      :  ?comparator:('k, 'cmp) Core_kernel.Map.comparator
      -> ('k * 'input, 'model, 'result) t
      -> ( ('k, 'input, 'cmp) Core_kernel.Map.t
         , 'model
         , ('k, 'result, 'cmp) Core_kernel.Map.t )
           t

    (** Given two components that produce maps, create a new component that produces a map
        that is merged according provided function [f]. *)
    val merge
      :  ('input, 'model, ('k, 'r1, 'cmp) Base.Map.t) t
      -> ('input, 'model, ('k, 'r2, 'cmp) Base.Map.t) t
      -> f:(key:'k
            -> [ `Both of 'r1 * 'r2 | `Left of 'r1 | `Right of 'r2 ]
            -> 'result option)
      -> ('input, 'model, ('k, 'result, 'cmp) Base.Map.t) t
  end

  module Option : sig
    val wrap_model
      :  ?on_action_for_none:(unit, 'model option) on_action_mismatch
      -> ('input, 'model, 'result) t
      -> ('input, 'model option, 'result option) t

    val wrap_model_with_default
      :  ?on_action_for_none:(unit, 'model option) on_action_mismatch
      -> ('input, 'model, 'result) t
      -> default:'result
      -> ('input, 'model option, 'result) t

  end

  module Either : sig
    val wrap_model
      :  ?on_action_for_other_component:( [ `Action_for_first of 'm2
                                          | `Action_for_second of 'm1
                                          ]
                                        , ('m1, 'm2) Core_kernel.Either.t )
           on_action_mismatch
      -> ('input, 'm1, 'r1) t
      -> ('input, 'm2, 'r2) t
      -> ('input, ('m1, 'm2) Core_kernel.Either.t, ('r1, 'r2) Core_kernel.Either.t) t

    val wrap_model_with_same_result
      :  ?on_action_for_other_component:( [ `Action_for_first of 'm2
                                          | `Action_for_second of 'm1
                                          ]
                                        , ('m1, 'm2) Core_kernel.Either.t )
           on_action_mismatch
      -> ('input, 'm1, 'r) t
      -> ('input, 'm2, 'r) t
      -> ('input, ('m1, 'm2) Core_kernel.Either.t, 'r) t

  end

  module List_deprecated : sig
    (** Projecting over lists in Bonsai is fraught with issues.
        1. Incremental can't be optimized for linked-list data structures.
        2. Using list-index as the method for routing actions to components is basically
        never what you actually want, causing mis-delivery of events every time that the
        list changes. *)
  end

  module Arrow : sig
    (** [('i, _, 'r) t] is an arrow from ['i] to ['r]. *)

    (** [arr] is the same as [pure]. *)
    val arr : ('input -> 'result) -> ('input, _, 'result) t

    (** [first t] applies [t] to the first part of the input.

        {v
                                    .-----------------------.
                                    | .-------------------. |
                       .-- 'input --+-| 'input -> 'result |-+-- 'result --.
                      /             | `-------------------` |              \
       'input * 'a --+---------'a --+-----------------------+-- 'a ---------+-- 'result * 'a
                                    `-----------------------`
       v} *)
    val first : ('input, 'model, 'result) t -> ('input * 'a, 'model, 'result * 'a) t

    (** [second t] applies [t] to the second part of the input.

        {v
                                   .-----------------------.
                       .----- 'a --+-----------------------+-- 'a -----.
                      /            | .-------------------. |            \
       'a * 'input --+--- 'input --+-| 'input -> 'result |-+-- 'result --+-- 'a * 'result
                                   | `-------------------` |
                                   `-----------------------`
       v} *)
    val second : ('input, 'model, 'result) t -> ('a * 'input, 'model, 'a * 'result) t

    (** [split t u] applies [t] to the first part of the input and [u] to the second
        part.


        {v
                                     .-----------------.
                                     | .-------------. |
                           .-- 'i1 --+-| 'i1 -> 'r1  |-+-- 'r1 --.
                          /          | `-------------` |          \
                         /           | .-------------. |           \
            'i1 * 'i2 --+----- 'i2 --+-| 'i2 -> 'r2  |-+-- 'r2 -----+-- 'r1 * 'r2
                                     | `-------------` |
                                     `-----------------`
       v} *)
    val split
      :  ('i1, 'model, 'r1) t
      -> ('i2, 'model, 'r2) t
      -> ('i1 * 'i2, 'model, 'r1 * 'r2) t

    (** [extend_first] returns the result of a bonsai component alongside its input.

        {v
                .----------------------------.
                |      .-------------------. |
       'input --+-+----| 'input -> 'result |-+-- 'result --.
                |  \   `-------------------` |              \
                |   `------------------------+-- 'input -----+-- 'result * 'input
                `----------------------------`
       v} *)
    val extend_first
      :  ('input, 'model, 'result) t
      -> ('input, 'model, 'result * 'input) t

    (** [extend_second] returns the result of a bonsai component alongside its input.

        {v
                .----------------------------.
                |   .------------------------+-- 'input --.
                |  /   .-------------------. |             \
       'input --+-+----| 'input -> 'result |-+-- 'result ---+-- 'input * 'result
                |      `-------------------` |
                `----------------------------`
       v} *)
    val extend_second
      :  ('input, 'model, 'result) t
      -> ('input, 'model, 'input * 'result) t

    (** [t *** u = split t u]. *)
    val ( *** )
      :  ('i1, 'model, 'r1) t
      -> ('i2, 'model, 'r2) t
      -> ('i1 * 'i2, 'model, 'r1 * 'r2) t

    (** [fanout t u] applies [t] and [u] to the same input and returns both results.  It's
        actually just [both].

        {v
                .------------------------.
                |      .---------------. |
                |   .--| 'input -> 'r1 |-+-- 'r1 --.
                |  /   `---------------` |          \
       'input --+-+                      |           \
                |  \   .---------------. |            \
                |   `--| 'input -> 'r2 |-+-- 'r2 ------+-- 'r1 * 'r2
                |      `---------------` |
                `------------------------`
       v} *)

    val fanout
      :  ('input, 'model, 'r1) t
      -> ('input, 'model, 'r2) t
      -> ('input, 'model, 'r1 * 'r2) t

    (** [t &&& u = fanout t u]. *)
    val ( &&& )
      :  ('input, 'model, 'r1) t
      -> ('input, 'model, 'r2) t
      -> ('input, 'model, 'r1 * 'r2) t

    (** [^>>] is the same as [@>>], but with a Haskell-like name. *)
    val ( ^>> ) : ('i1 -> 'i2) -> ('i2, 'model, 'result) t -> ('i1, 'model, 'result) t

    (** [>>^] is the same as [>>|], but with a Haskell-like name. *)
    val ( >>^ ) : ('input, 'model, 'r1) t -> ('r1 -> 'r2) -> ('input, 'model, 'r2) t
  end

  module With_incr : sig
    (** Constructs a bonsai component whose result is always
        the same as its input Incremental node.  *)
    val of_incr : 'result Incr.t -> (_, _, 'result) t

    (** Same as [Bonsai.pure] but allows the user to optimize using Incremental. *)
    val pure : f:('input Incr.t -> 'result Incr.t) -> ('input, _, 'result) t

    (** Same as [Bonsai.pure_from_model], but allows the user to optimize using Incremental. *)
    val pure_from_model : f:('model Incr.t -> 'result Incr.t) -> (_, 'model, 'result) t

    (** Creates a bonsai component where the given cutoff is applied to the incremental
        node for the component's model, preventing a component from being recalculated
        unnecessarily.

        See [Incr.set_cutoff]. *)
    val model_cutoff
      :  ('input, 'model, 'result) t
      -> cutoff:'model Incremental.Cutoff.t
      -> ('input, 'model, 'result) t

    (** Creates a bonsai component where the given cutoff is applied to the incremental
        node as input passes through the component, preventing a component from
        being recalculated unnecessarily.

        See [Incr.set_cutoff]. *)
    val value_cutoff : cutoff:'input Incremental.Cutoff.t -> ('input, _, 'input) t

    (** Transforms the result of a component, exposing the incrementality for optimization
        purposes. *)
    val map
      :  ('input, 'model, 'r1) t
      -> f:('r1 Incr.t -> 'r2 Incr.t)
      -> ('input, 'model, 'r2) t

    (** Transforms the input of a component, exposing the incrementality for optimization
        purposes. The signature of [f] is reversed from most other map functions. *)
    val map_input
      :  ('i2, 'model, 'result) t
      -> f:('i1 Incr.t -> 'i2 Incr.t)
      -> ('i1, 'model, 'result) t

    module type S = sig
      module Input : T
      module Model : T

      module Action : sig
        type t [@@deriving sexp_of]
      end

      module Result : T

      val apply_action
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> (schedule_event:(Event.t -> unit) -> Action.t -> Model.t) Incr.t

      val compute
        :  Input.t Incr.t
        -> Model.t Incr.t
        -> inject:(Action.t -> Event.t)
        -> Result.t Incr.t

      (** A name to identify this component.

          A reasonable fallback is [Source_code_position.to_string [%here]]. *)
      val name : string
    end

    type ('input, 'model, 'action, 'result) component_s =
      (module S
        with type Input.t = 'input
         and type Model.t = 'model
         and type Action.t = 'action
         and type Result.t = 'result)

    val of_module
      :  ('input, 'model, 'action, 'result) component_s
      -> ('input, 'model, 'result) t
  end
end

module type Bonsai = sig
  module Generic : sig
    type ('extra, 'new_model) on_action_mismatch =
      [ `Ignore
      | `Raise
      | `Warn
      | `Custom of 'extra -> 'new_model
      ]

    (** The component type ([('input, 'model, 'result) Bonsai.t]) can be thought of as a
        function from ['model] to ['result], but where the ['result] can schedule events of
        the component's "action type".  These actions are used to produce a new ['model]
        which in turn causes the ['result] to be recomputed. Instances of the ['result] type
        can contain callbacks which schedule actions when interacted with by user (via
        button click, text input, etc). These actions are handled by the component's
        [apply_action] function, which yields a new model to be displayed. *)
    type ('input, 'model, 'result, 'incr, 'event) t =
      ('input, 'model, 'result, 'incr, 'event) Bonsai_types.Packed.t

    type ('input, 'model, 'result, 'incr, 'event) nonexpert_t :=
      ('input, 'model, 'result, 'incr, 'event) t

    val sexp_of_t : (_, _, _, _, _) t -> Sexp.t

    (** {1 Component Constructors} *)

    (** Returns a component with no action or model, only a constant result. *)
    val const : 'result -> (_, _, 'result, 'incr, _) t

    (** A pure function with no model from 'input to 'result *)
    val pure : f:('input -> 'result) -> ('input, _, 'result, _, _) t

    (** Like [pure], but gets the input from the ['model]. *)
    val pure_from_model : f:('model -> 'result) -> (_, 'model, 'result, _, _) t


    (** Creates a leaf-node on the Bonsai tree.  A leaf node
        has computation (via the [compute] parameter) and behavior
        (via the [apply_action] parameter).

        Additionally [name] and [sexp_of_action] are provided to add some
        hooks for improved debugability. *)
    val leaf
      :  apply_action:(inject:('action -> 'event)
                       -> schedule_event:('event -> unit)
                       -> 'input
                       -> 'model
                       -> 'action
                       -> 'model)
      -> compute:(inject:('action -> 'event) -> 'input -> 'model -> 'result)
      -> name:string
      -> sexp_of_action:('action -> Sexp.t)
      -> ('input, 'model, 'result, _, 'event) t

    (** [compose a b] joins [a] and [b] together such that the result of [a] is used as the
        input of [b]. *)
    val compose
      :  ('i1, 'model, 'r1, 'incr, 'event) t
      -> ('r1, 'model, 'r2, 'incr, 'event) t
      -> ('i1, 'model, 'r2, 'incr, 'event) t

    module type Enum = sig
      type t [@@deriving sexp_of, compare, enumerate]
    end

    val enum
      :  ?on_action_mismatch:('key * 'model, 'model) on_action_mismatch
      -> (module Enum with type t = 'key)
      -> which:('input -> 'model -> 'key)
      -> handle:('key -> ('input, 'model, 'result, 'incr, 'event) t)
      -> ('input, 'model, 'result, 'incr, 'event) t

    (** [if_] is a simple application of [enum] to [(module Bool)]. *)
    val if_
      :  ?on_action_mismatch:(bool * 'model, 'model) on_action_mismatch
      -> ('input -> 'model -> bool)
      -> then_:('input, 'model, 'result, 'incr, 'event) t
      -> else_:('input, 'model, 'result, 'incr, 'event) t
      -> ('input, 'model, 'result, 'incr, 'event) t

    module Infix : sig
      (** [a >>> b] is [compose a b] *)
      val ( >>> )
        :  ('i1, 'model, 'r1, 'incr, 'event) t
        -> ('r1, 'model, 'r2, 'incr, 'event) t
        -> ('i1, 'model, 'r2, 'incr, 'event) t

      val ( >>| )
        :  ('input, 'model, 'r1, 'incr, 'event) t
        -> ('r1 -> 'r2)
        -> ('input, 'model, 'r2, 'incr, 'event) t

      val ( @>> )
        :  ('i1 -> 'i2)
        -> ('i2, 'model, 'result, 'incr, 'event) t
        -> ('i1, 'model, 'result, 'incr, 'event) t
    end

    val map
      :  ('input, 'model, 'r1, 'incr, 'event) t
      -> f:('r1 -> 'r2)
      -> ('input, 'model, 'r2, 'incr, 'event) t

    (*_ include Applicative.S3 with type ('r, 'i, 'm) t := ('i, 'm, 'r) t *)

    (** Transforms the input of a component. The signature of [f] is reversed from most
        other map functions. *)
    val map_input
      :  ('i2, 'model, 'result, 'incr, 'event) t
      -> f:('i1 -> 'i2)
      -> ('i1, 'model, 'result, 'incr, 'event) t

    module Let_syntax : sig
      val return : 'result -> (_, _, 'result, _, _) t

      include module type of Infix

      module Let_syntax : sig
        val return : 'result -> (_, _, 'result, _, _) t

        val map
          :  ('input, 'model, 'r1, 'incr, 'event) t
          -> f:('r1 -> 'r2)
          -> ('input, 'model, 'r2, 'incr, 'event) t

        val both
          :  ('input, 'model, 'r1, 'incr, 'event) t
          -> ('input, 'model, 'r2, 'incr, 'event) t
          -> ('input, 'model, 'r1 * 'r2, 'incr, 'event) t

        module Open_on_rhs : module type of Infix
      end
    end

    module Expert : sig
      (** The underlying representations of Bonsai components.  This module is provided "as
          is", without warranty of any kind, express or implied... *)

      (** [unpacked] exposes the action type of a component as an existential. *)
      type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

      (** Every Bonsai component has a hidden ['action] type, which can be revealed as an
          existential by pattern matching on {!t}. *)
      type ('input, 'model, 'result, 'incr, 'event) t = private
        | T :
            ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
            * 'action Type_equal.Id.t
            -> ('input, 'model, 'result, 'incr, 'event) t

      (** [reveal t] is just the identity, but it allows you to pattern match on the GADT to
          get the unpacked component (which you can [eval]) and the action's type ID. *)
      val reveal
        :  ('input, 'model, 'result, 'incr, 'event) nonexpert_t
        -> ('input, 'model, 'result, 'incr, 'event) t

      (** [conceal] is the inverse of [reveal]. *)
      val conceal
        :  ('input, 'model, 'result, 'incr, 'event) t
        -> ('input, 'model, 'result, 'incr, 'event) nonexpert_t


      (** Builds a component out of the incremental function from ['model Incr.t] to
          [Snapshot.t Incr.t]. This function is in the expert module because it is
          effectively a black box to the rest of the Bonsai library, which means [optimize]
          cannot do anything with it, for example. *)
      val of_full
        :  Source_code_position.t
        -> f:(input:('input, 'incr) Incremental.t
              -> old_model:('model option, 'incr) Incremental.t
              -> model:('model, 'incr) Incremental.t
              -> inject:('action -> 'event)
              -> incr_state:'incr Incremental.State.t
              -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t)
        -> action_type_id:'action Type_equal.Id.t
        -> ('input, 'model, 'result, 'incr, 'event) t

      (*_ Do you like GADT's? I do. That's why this function is called [eval], and not
        something that is more informative. Gotta keep those traditions alive somehow. *)

      (** Walks the component tree and builds a [Snapshot.t Incr.t] which will be recomputed
          when [model] and [old_model] change. *)
      val eval
        :  input:('input, 'incr) Incremental.t
        -> old_model:('model option, 'incr) Incremental.t
        -> model:('model, 'incr) Incremental.t
        -> inject:('action -> 'event)
        -> action_type_id:'action Type_equal.Id.t
        -> incr_state:'incr Incremental.State.t
        -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
        -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

      (** Performs constant-folding and other optimizations on the component tree. This
          function is called once-per-component tree, which should be roughly
          once-per-application life-cycle unless there is dynamic component tree creation
          going on (perhaps via [of_full] and [eval]). *)
      val optimize
        :  ('input, 'model, 'result, 'incr, 'event) t
        -> ('input, 'model, 'result, 'incr, 'event) t

      module Snapshot = Snapshot
    end

    (** {1 Combinators} *)

    module Model : sig
      val f
        :  ('input, 'm1, 'result, 'incr, 'event) t
        -> get:('m2 -> 'm1)
        -> set:('m2 -> 'm1 -> 'm2)
        -> ('input, 'm2, 'result, 'incr, 'event) t

      val ignore
        :  ('input, unit, 'result, 'incr, 'event) t
        -> ('input, 'model, 'result, 'incr, 'event) t

      val field
        :  ('model_outer, 'model_inner) Field.t
        -> ('input, 'model_inner, 'result, 'incr, 'event) t
        -> ('input, 'model_outer, 'result, 'incr, 'event) t

      val state_machine
        :  Source_code_position.t
        -> sexp_of_action:('action -> Sexp.t)
        -> apply_action:(inject:('action -> 'event)
                         -> schedule_event:('event -> unit)
                         -> 'input
                         -> 'model
                         -> 'action
                         -> 'model)
        -> ('input, 'model, 'action -> 'event, 'incr, 'event) t

      (** Takes a component that operates immutably over an input (via 'input), and
          converts it to a component that gets its input from the model. *)
      val to_input
        :  ('input, unit, 'result, 'incr, 'event) t
        -> (unit, 'input, 'result, 'incr, 'event) t

      (** Takes a component that operates immutably over an input (via 'input), and
          converts it to a component that gets part of that input from the model. *)
      val to_input_with_other
        :  ('input * 'model, unit, 'result, 'incr, 'event) t
        -> ('input, 'model, 'result, 'incr, 'event) t
    end

    module Map : sig
      (** Transforms a component into a new component whose model and result are maps. *)
      val assoc_model
        :  ?comparator:('k, 'cmp) Map.comparator
        -> ('input, 'model, 'result, 'incr, 'event) t
        -> ('input, ('k, 'model, 'cmp) Map.t, ('k, 'result, 'cmp) Map.t, 'incr, 'event) t

      (** Transforms a component into a new component whose model and result are maps.  The
          input for the transformed component also receives its key as an input. *)
      val associ_model
        :  ?comparator:('k, 'cmp) Map.comparator
        -> ('k * 'input, 'model, 'result, 'incr, 'event) t
        -> ('input, ('k, 'model, 'cmp) Map.t, ('k, 'result, 'cmp) Map.t, 'incr, 'event) t

      (** Transforms a component into a new component whose input and result are maps. *)
      val assoc_input
        :  ?comparator:('k, 'cmp) Map.comparator
        -> ('input, 'model, 'result, 'incr, 'event) t
        -> (('k, 'input, 'cmp) Map.t, 'model, ('k, 'result, 'cmp) Map.t, 'incr, 'event) t

      (** Transforms a component into a new component whose input and result are maps.
          The input for the transformed component also receives its key as an input.  *)
      val associ_input
        :  ?comparator:('k, 'cmp) Map.comparator
        -> ('k * 'input, 'model, 'result, 'incr, 'event) t
        -> (('k, 'input, 'cmp) Map.t, 'model, ('k, 'result, 'cmp) Map.t, 'incr, 'event) t

      (** Given two components that produce maps, create a new component that produces a map
          that is merged according provided function [f]. *)
      val merge
        :  ('input, 'model, ('k, 'r1, 'cmp) Base.Map.t, 'incr, 'event) t
        -> ('input, 'model, ('k, 'r2, 'cmp) Base.Map.t, 'incr, 'event) t
        -> f:(key:'k
              -> [ `Both of 'r1 * 'r2 | `Left of 'r1 | `Right of 'r2 ]
              -> 'result option)
        -> ('input, 'model, ('k, 'result, 'cmp) Base.Map.t, 'incr, 'event) t
    end

    module Option : sig
      val wrap_model
        :  ?on_action_for_none:(unit, 'model option) on_action_mismatch
        -> ('input, 'model, 'result, 'incr, 'event) t
        -> ('input, 'model option, 'result option, 'incr, 'event) t

      val wrap_model_with_default
        :  ?on_action_for_none:(unit, 'model option) on_action_mismatch
        -> ('input, 'model, 'result, 'incr, 'event) t
        -> default:'result
        -> ('input, 'model option, 'result, 'incr, 'event) t

    end

    module Either : sig
      val wrap_model
        :  ?on_action_for_other_component:( [ `Action_for_first of 'm2
                                            | `Action_for_second of 'm1
                                            ]
                                          , ('m1, 'm2) Either.t )
             on_action_mismatch
        -> ('input, 'm1, 'r1, 'incr, 'event) t
        -> ('input, 'm2, 'r2, 'incr, 'event) t
        -> ('input, ('m1, 'm2) Either.t, ('r1, 'r2) Either.t, 'incr, 'event) t

      val wrap_model_with_same_result
        :  ?on_action_for_other_component:( [ `Action_for_first of 'm2
                                            | `Action_for_second of 'm1
                                            ]
                                          , ('m1, 'm2) Either.t )
             on_action_mismatch
        -> ('input, 'm1, 'r, 'incr, 'event) t
        -> ('input, 'm2, 'r, 'incr, 'event) t
        -> ('input, ('m1, 'm2) Either.t, 'r, 'incr, 'event) t

    end

    module List_deprecated : sig
      (** Projecting over lists in Bonsai is fraught with issues.
          1. Incremental can't be optimized for linked-list data structures.
          2. Using list-index as the method for routing actions to components is basically
          never what you actually want, causing mis-delivery of events every time that the
          list changes. *)
    end

    module Arrow : sig
      (** [('i, _, 'r) t] is an arrow from ['i] to ['r]. *)

      (** [arr] is the same as [pure]. *)
      val arr : ('input -> 'result) -> ('input, _, 'result, _, _) t

      (** [first t] applies [t] to the first part of the input. *)
      val first
        :  ('input, 'model, 'result, 'incr, 'event) t
        -> ('input * 'a, 'model, 'result * 'a, 'incr, 'event) t

      (** [second t] applies [t] to the second part of the input. *)
      val second
        :  ('input, 'model, 'result, 'incr, 'event) t
        -> ('a * 'input, 'model, 'a * 'result, 'incr, 'event) t

      (** [split t u] applies [t] to the first part of the input and [u] to the second
          part. *)
      val split
        :  ('i1, 'model, 'r1, 'incr, 'event) t
        -> ('i2, 'model, 'r2, 'incr, 'event) t
        -> ('i1 * 'i2, 'model, 'r1 * 'r2, 'incr, 'event) t

      (** [t *** u = split t u]. *)
      val ( *** )
        :  ('i1, 'model, 'r1, 'incr, 'event) t
        -> ('i2, 'model, 'r2, 'incr, 'event) t
        -> ('i1 * 'i2, 'model, 'r1 * 'r2, 'incr, 'event) t

      (** [fanout t u] applies [t] and [u] to the same input and returns both results.  It's
          actually just [both]. *)
      val fanout
        :  ('input, 'model, 'r1, 'incr, 'event) t
        -> ('input, 'model, 'r2, 'incr, 'event) t
        -> ('input, 'model, 'r1 * 'r2, 'incr, 'event) t

      (** [t &&& u = fanout t u]. *)
      val ( &&& )
        :  ('input, 'model, 'r1, 'incr, 'event) t
        -> ('input, 'model, 'r2, 'incr, 'event) t
        -> ('input, 'model, 'r1 * 'r2, 'incr, 'event) t

      (** [^>>] is the same as [@>>], but with a Haskell-like name. *)
      val ( ^>> )
        :  ('i1 -> 'i2)
        -> ('i2, 'model, 'result, 'incr, 'event) t
        -> ('i1, 'model, 'result, 'incr, 'event) t

      (** [>>^] is the same as [>>|], but with a Haskell-like name. *)
      val ( >>^ )
        :  ('input, 'model, 'r1, 'incr, 'event) t
        -> ('r1 -> 'r2)
        -> ('input, 'model, 'r2, 'incr, 'event) t
    end

    module With_incr : sig
      (** Constructs a bonsai component whose result is always
          the same as this Incremental node.  *)
      val of_incr : ('result, 'incr) Incremental.t -> (_, _, 'result, 'incr, _) t

      (** Same as [Bonsai.pure] but allows the user to optimize using Incremental. *)
      val pure
        :  f:(('input, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
        -> ('input, _, 'result, 'incr, _) t

      (** Same as [Bonsai.pure_from_model], but allows the user to optimize using Incremental. *)
      val pure_from_model
        :  f:(('model, 'incr) Incremental.t -> ('result, 'incr) Incremental.t)
        -> (_, 'model, 'result, 'incr, _) t

      (** Same as [Bonsai.leaf], but incremental *)
      val leaf
        :  apply_action:(('input, 'incr) Incremental.t
                         -> ('model, 'incr) Incremental.t
                         -> inject:('action -> 'event)
                         -> ( schedule_event:('event -> unit) -> 'action -> 'model
                            , 'incr )
                              Incremental.t)
        -> compute:(('input, 'incr) Incremental.t
                    -> ('model, 'incr) Incremental.t
                    -> inject:('action -> 'event)
                    -> ('result, 'incr) Incremental.t)
        -> name:string
        -> sexp_of_action:('action -> Sexp.t)
        -> ('input, 'model, 'result, 'incr, 'event) t

      (** Creates a bonsai component where the given cutoff is applied to the incremental
          node for the component's model, preventing a component from being recalculated
          unnecessarily.

          See [Incr.set_cutoff]. *)
      val model_cutoff
        :  ('input, 'model, 'result, 'incr, 'event) t
        -> cutoff:'model Incremental.Cutoff.t
        -> ('input, 'model, 'result, 'incr, 'event) t

      val value_cutoff
        :  cutoff:'input Incremental.Cutoff.t
        -> ('input, _, 'input, _, _) t

      (** Transforms the result of a component, exposing the incrementality for optimization
          purposes. *)
      val map
        :  ('input, 'model, 'r1, 'incr, 'event) t
        -> f:(('r1, 'incr) Incremental.t -> ('r2, 'incr) Incremental.t)
        -> ('input, 'model, 'r2, 'incr, 'event) t

      (** Transforms the input of a component, exposing the incrementality for optimization
          purposes. The signature of [f] is reversed from most other map functions. *)
      val map_input
        :  ('i2, 'model, 'result, 'incr, 'event) t
        -> f:(('i1, 'incr) Incremental.t -> ('i2, 'incr) Incremental.t)
        -> ('i1, 'model, 'result, 'incr, 'event) t
    end
  end

  module type S_gen = S_gen

  module type S = sig
    module Incr : Incremental.S
    module Event : Event.S
    include S_gen with module Incr := Incr with module Event := Event

    val to_generic
      :  ('i, 'm, 'r) t
      -> ('i, 'm, 'r, Incr.state_witness, Event.t) Generic.t

    val of_generic
      :  ('i, 'm, 'r, Incr.state_witness, Event.t) Generic.t
      -> ('i, 'm, 'r) t
  end

  module Make (Incr : Incremental.S) (Event : Event.S) :
    S with module Incr := Incr and module Event := Event
end
