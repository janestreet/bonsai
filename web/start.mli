(** [Start] handles the entire lifecycle of web-based Bonsai application.

    Put another way, [Start] is like {!Core.Command} for Bonsai_web apps. *)

open! Core_kernel
open! Async_kernel
open! Import

module Handle : sig
  (** A handle represents a running app, and can be used to schedule external actions,
      determine if the app has started, and stop the app. *)
  type ('input, 'incoming, 'outgoing) t

  val stop : _ t -> unit
  val started : _ t -> unit Deferred.t
  val schedule : (_, 'incoming, _) t -> 'incoming -> unit
  val input : ('input, _, _) t -> 'input
  val set_input : ('input, _, _) t -> 'input -> unit
  val update_input : ('input, _, _) t -> f:('input -> 'input) -> unit
  val outgoing : (_, _, 'outgoing) t -> 'outgoing Pipe.Reader.t
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
  type 'incoming t

  val create
    :  view:Vdom.Node.t
    -> inject_incoming:('incoming -> Vdom.Event.t)
    -> 'incoming t
end

(** {1 Start functions} *)

(** Start an application, receiving a handle that can't schedule any actions.

    The outermost [Bonsai.t]'s result type parameter should be a [Vdom.Node.t], which will
    be bound to the DOM element with id [bind_to_element_with_id]. *)
val start_standalone
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> ('input, 'model, Vdom.Node.t) Bonsai.t
  -> ('input, Nothing.t, Nothing.t) Handle.t

(** Start an application, receiving a handle that can schedule actions of a user-defined
    type.

    The outermost [Bonsai.t]'s result type parameter should be a pair consisting of:

    - a [Vdom.Node.t], which will be bound to the DOM element with id
      [bind_to_element_with_id]; and
    - an [inject] function that accepts external actions and returns [Vdom.Event.t]s. *)
val start
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> (('input, 'outgoing) App_input.t, 'model, 'incoming App_result.t) Bonsai.t
  -> ('input, 'incoming, 'outgoing) Handle.t
