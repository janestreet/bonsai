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
  val set_input : ('input, _, _) t -> 'input -> unit
  val outgoing : (_, _, 'outgoing) t -> 'outgoing Pipe.Reader.t
end

(** {1 Start functions} *)

(** Start an application, receiving a handle that can't schedule any actions.

    The outermost [Bonsai.t]s result type parameter should be a [Vdom.Node.t], which will
    be bound to the DOM element with id [bind_to_element_with_id]. *)
val start_standalone
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> ('input, 'model, Vdom.Node.t) Bonsai.t
  -> ('input, Nothing.t, Nothing.t) Handle.t

(** Start an application, receiving a handle that can schedule actions of a user-defined
    type.

    The outermost [Bonsai.t]s result type parameter should be pair of:

    - a [Vdom.Node.t], which will be bound to the DOM element with id
      [bind_to_element_with_id]; and
    - an [inject] function that accepts external actions and returns [Vdom.Event.t]s. *)
val start
  :  initial_input:'input
  -> initial_model:'model
  -> bind_to_element_with_id:string
  -> ( 'input * ('outgoing -> Vdom.Event.t)
     , 'model
     , Vdom.Node.t * ('incoming -> Vdom.Event.t) )
       Bonsai.t
  -> ('input, 'incoming, 'outgoing) Handle.t
