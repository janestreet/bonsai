(** [Start] handles the entire lifecycle of web-based Bonsai application.

    Put another way, [Start] is like {!Core.Command} for Bonsai_web apps. *)

open! Core_kernel
open! Async_kernel
open! Import

module Handle : sig
  (** A handle represents a running app, and can be used to schedule external actions,
      determine if the app has started, and stop the app.

      - ['input] is the input type of the outermost component of the app.
      - ['extra] is any extra result (other than the app view) returned by the outermost
        component. *)
  type ('input, 'extra, 'incoming, 'outgoing) t

  (** Stops the application from updating or responding to any actions.
      The app cannot be restarted.  The displayed contents are not removed from
      the page. *)
  val stop : _ t -> unit

  (** A deferred that resolves after the first display occurs.  At this
      point, there is content being shown on the screen. *)
  val started : _ t -> unit Deferred.t

  (** Schedules an event for an action of type ['incoming].  This
      action gets routed to the Bonsai component that returned its
      [inject] function in the [App_result.t] *)
  val schedule : (_, _, 'incoming, _) t -> 'incoming -> unit

  (** Returns a pipe containing all of the actions of type ['outgoing]
      that the Bonsai app has scheduled. *)
  val outgoing : (_, _, _, 'outgoing) t -> 'outgoing Pipe.Reader.t

  (** Returns the value of the current input. *)
  val input : ('input, _, _, _) t -> 'input

  (** Overwrites the current input. *)
  val set_input : ('input, _, _, _) t -> 'input -> unit

  (** A helper method for fetching the current input and providing an
      updating function that returns a new input.  This is to make
      functionally updating data structures easier. *)
  val update_input : ('input, _, _, _) t -> f:('input -> 'input) -> unit

  (** A bus containing successive values of [extra] returned by the app in its
      [App_result.t]. *)
  val extra : (_, 'extra, _, _) t -> ('extra -> unit) Bus.Read_only.t

  (** The latest value written to [extra], if it exists. *)
  val last_extra : (_, 'extra, _, _) t -> 'extra option
end

(** The input of an application-level component is a value of type
    [('input, 'outgoing) App_input.t]. *)
module App_input : sig
  type ('input, 'outgoing) t

  (** The [input] field is used to access the ['input] to the application that is
      set from the imperative [Handle.t] value. *)
  val input : ('input, _) t -> 'input

  (** [inject_outgoing] is used to inject values of the type ['outgoing] into events to
      communicate with the imperative [Handle.t] holder.  Any values injected via
      [inject_outgoing] will be present in the ['outgoing Pipe.Reader.t] that can be
      acquired via {!Handle.outgoing}. *)
  val inject_outgoing : (_, 'outgoing) t -> 'outgoing -> Vdom.Event.t
end

(** The result of an application-level component is a value of type ['incoming
    App_result.t].  This value contains the view of the app, and also an inject function:
    a way for the holder of the [Handle.t] to send events into the application component.

    If the application developer doesn't want to use incoming events, they should use
    {!Core_kernel.Nothing.t} for the ['incoming] type, and
    {!Core_kernel.Nothing.unreachable_code} for the value of [inject_incoming]. *)
module App_result : sig
  type ('extra, 'incoming) t

  val create
    :  view:Vdom.Node.t
    -> extra:'extra
    -> inject_incoming:('incoming -> Vdom.Event.t)
    -> ('extra, 'incoming) t
end

(** {1 Start functions} *)

(** Start an application, receiving a handle that can't schedule any actions.

    The outermost [Bonsai.t]'s result type parameter should be a [Vdom.Node.t], which will
    be bound to the DOM element with id [bind_to_element_with_id]. *)
val start_standalone
  :  initial_input:'input
  -> bind_to_element_with_id:string
  -> ('input, Vdom.Node.t) Bonsai.t
  -> ('input, unit, Nothing.t, Nothing.t) Handle.t

(** Start an application, receiving a handle that can schedule actions of a user-defined
    type.

    The outermost [Bonsai.t]'s result type parameter should be a pair consisting of:

    - a [Vdom.Node.t], which will be bound to the DOM element with id
      [bind_to_element_with_id]; and
    - an [inject] function that accepts external actions and returns [Vdom.Event.t]s. *)
val start
  :  initial_input:'input
  -> bind_to_element_with_id:string
  -> (('input, 'outgoing) App_input.t, ('extra, 'incoming) App_result.t) Bonsai.t
  -> ('input, 'extra, 'incoming, 'outgoing) Handle.t

