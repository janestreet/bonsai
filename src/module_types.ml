open! Core
module Incr = Ui_incr
module Effect = Ui_effect

module type Comparator = sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module Comparator with type t = 'k and type comparator_witness = 'cmp)

module type Enum = sig
  type t [@@deriving compare, enumerate, equal, sexp]
end

module type Model = sig
  type t [@@deriving sexp, equal]
end

module type Action = sig
  type t [@@deriving sexp_of]
end

(** Many modules have the same shape: they declare the model, action, and result of the
    component, and then define [apply_action] and [compute] over those types.

    This is intended to be used with the {!of_module} function. *)
module type Component_s = sig
  (** The name of the component.  This is used to identify the component while
      debugging, and to annotate error messages.  If you can't think of a good
      name, a reasonable fallback is [Source_code_position.to_string [%here]]. *)
  val name : string

  (** A component receives read-only input, either from the output of other components or
      from an external system (e.g. data from a server).  The input is frequently dynamic,
      but may also be constant. *)
  module Input : sig
    type t
  end

  (** A component's "model" is a state machine that the component can both read and write
      to.  Because both the input and model are readable, it can be hard to decide whether
      to request some data from the input or the model.  It is highly recommended to put
      only the data that needs mutation in [Model.t], and the rest in [Input.t]. *)
  module Model : Model

  (** Components can change their own model by issuing "actions" that perform the state
      transition.  If you think of the state machine as having states of type [Model.t],
      then the transitions between those nodes would be of type [Action.t]. *)
  module Action : Action

  (** While UI components stereotypically produce some kind of "view", with
      Bonsai, components are small and easy enough to compose that Bonsai
      components frequently produce intermediate results which are then wired into
      other components. *)
  module Result : sig
    type t
  end

  (** When an action is raised by this component (via an Event.t), Bonsai will eventually
      pass that action back to this component's [apply_action] function.  This function is
      responsible for producing a new model given the current model and the incoming
      action.

      [apply_action] may emit further actions via [schedule_event] or use Async to arrange
      for [schedule_event] to be called later. *)
  val apply_action
    :  inject:(Action.t -> unit Ui_effect.t)
    -> schedule_event:(unit Ui_effect.t -> unit)
    -> Input.t
    -> Model.t
    -> Action.t
    -> Model.t

  (** [compute] is a function from input and model to the component's result.
      In a component that produces a view, this function could be thought of as the
      "view computation function".

      This function is also given an "inject" function which converts this component's
      [Action.t] to a global [Event.t] which can be given to Bonsai to schedule.
      Frequently, [inject] (or some wrapper around it) is included in the result as a
      handler for some kind of user input.

      For example, the result may contain a Vdom button node that has an "on click"
      handler which calls [inject] to trigger an action. *)
  val compute : inject:(Action.t -> unit Ui_effect.t) -> Input.t -> Model.t -> Result.t
end

type ('input, 'model, 'action, 'result) component_s =
  (module Component_s
    with type Input.t = 'input
     and type Model.t = 'model
     and type Action.t = 'action
     and type Result.t = 'result)

(** This module type is similar to {!Component_s}, except that many of the functions'
    arguments and return values are incremental.  See {!Component_s} for an explanation of
    the items in this signature. *)
module type Component_s_incr = sig
  val name : string

  module Input : sig
    type t
  end

  module Model : Model
  module Action : Action

  module Result : sig
    type t
  end

  val apply_action
    :  Input.t Incr.t
    -> inject:(Action.t -> unit Ui_effect.t)
    -> (schedule_event:(unit Ui_effect.t -> unit) -> Model.t -> Action.t -> Model.t)
         Incr.t

  val compute
    :  Input.t Incr.t
    -> Model.t Incr.t
    -> inject:(Action.t -> unit Ui_effect.t)
    -> Result.t Incr.t
end

type ('input, 'model, 'action, 'result) component_s_incr =
  (module Component_s_incr
    with type Input.t = 'input
     and type Model.t = 'model
     and type Action.t = 'action
     and type Result.t = 'result)

module type Mapn = sig
  type 'a t

  val map3 : 'a1 t -> 'a2 t -> 'a3 t -> f:('a1 -> 'a2 -> 'a3 -> 'b) -> 'b t

  val map4
    :  'a1 t
    -> 'a2 t
    -> 'a3 t
    -> 'a4 t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
    -> 'b t

  val map5
    :  'a1 t
    -> 'a2 t
    -> 'a3 t
    -> 'a4 t
    -> 'a5 t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
    -> 'b t

  val map6
    :  'a1 t
    -> 'a2 t
    -> 'a3 t
    -> 'a4 t
    -> 'a5 t
    -> 'a6 t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
    -> 'b t

  val map7
    :  'a1 t
    -> 'a2 t
    -> 'a3 t
    -> 'a4 t
    -> 'a5 t
    -> 'a6 t
    -> 'a7 t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b)
    -> 'b t
end
