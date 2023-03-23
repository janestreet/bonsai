open! Core
open! Js_of_ocaml
open! Bonsai.For_open
open! Virtual_dom

module type S = sig
  type element = private #Dom_html.element
  type input
  type state

  (** The [init] function is used to construct the widget's initial state and the
      dom element that it is tied to.  It is passed the input for the widget, alongside
      a [get_input] function that will always return the most recent input passed to the
      widget.  You can close over [get_input] or store it in your [state]. *)
  val init : get_input:(unit -> input) -> input -> state * element Js.t

  (** Update is called whenever the input to the widget changes.  The most recent input
      value that was passed to the widget is available via [prev_input]. *)
  val update : prev_input:input -> input -> state -> element Js.t -> element Js.t

  (** [destroy] is called when the widget leaves the page.  You should do any resource
      cleanup here. *)
  val destroy : input -> state -> element Js.t -> unit
end

type ('input, 'state) t = private
  { view : Vdom.Node.t (** The view of the widget *)
  ; modify : ('input -> 'state -> unit) -> unit Effect.t
  (** A callback for modifying the widget.  The most recent inputs and the current state
      of the widgets are provided. *)
  ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
  (** [read] lets you look at the most recent input and current state of any instances
      of the widget. *)
  }

(** A component that makes it easy to build low-level components that make direct
    modifications to the DOM.  Because this component returns a [Vdom.Node.t] and that
    vdom node can be placed in multiple locations in the vdom, this component actually
    tracks a _set of instances_ of this widget.  This is why the [read] function produces
    a list of read values. *)
val component
  :  ?vdom_for_testing:('input -> Vdom.Node.t)
  -> (module S with type input = 'input and type state = 'state)
  -> 'input Value.t
  -> ('input, 'state) t Computation.t

module Low_level : sig
  (** A mutable-state tracker is meant to be used in concert with [Vdom.Node.widget] or
      [Vdom.Attr.create_hook].  Because a widget and hook can exist in multiple places in
      the dom at the same time, this state-tracker actually tracks a collection of states,
      which is why [read] returns a list, and the callback you pass to [modify] can get
      called multiple times per invocation.

      [unsafe_init] should be called inside the widget or hooks's [init] function and is
      passed some subset of the widget's state.  Then, you must store the returned Id.t in
      the widget's state.  If [unsafe_init] is called, then you _must_ call [unsafe_destroy]
      with the returned ID or you risk leaking memory, and making the output of calls to
      [read] return stale data.

      [unsafe_destroy] should be called inside the widget or hook's [destroy] function,
      and must be passed the same [id] that was returned from the corresponding [create].

      [modify] can be invoked to run some mutating function on the widget's state.  It is legal to
      use this function anywhere that an Effect can be scheduled.

      [read] provides access to the states of each instance managed by this mutable-state-tracker.
      You can use this function anywhere that an Effect can be scheduled. *)

  module Id : T

  type 's t =
    { unsafe_init : 's -> Id.t
    ; unsafe_destroy : Id.t -> unit
    ; modify : ('s -> unit) -> unit Effect.t
    ; read : 'a. ('s -> 'a) -> 'a list Effect.t
    }

  val component : unit -> 's t Computation.t
end
