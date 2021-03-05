open! Core_kernel
open! Import

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  (** [filter_printed_attributes] controls which attributes on a Node will get
      printed analyzing the string name of the attribute. Styles correspond to
      the string "style" and a Node's key corresponds to the string "@key" *)
  val vdom
    :  ?filter_printed_attributes:(string -> bool)
    -> ('a -> Vdom.Node.t)
    -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val click_on : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val input_text
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> text:string
    -> unit

  val focus : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit
  val blur : ('a, 'b) t -> get_vdom:('a -> Vdom.Node.t) -> selector:string -> unit

  val trigger_hook
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> ('c -> Vdom.Event.t) Type_equal.Id.t
    -> 'c
    -> unit

  module Drag_and_drop : sig
    (** Redeclare type to prevent shadowing *)
    type ('a, 'b) handle := ('a, 'b) t

    (** Represents the current state of the drag-and-drop process from the
        test's point of view; this is not the same as the internal
        drag-and-drop model. It is for the purpose of validating the sequence
        of actions given to [run] *)
    type t

    (** The initial state. This should be passed to the first call to [run]. *)
    val not_dragging : t

    (** The actions that a test can perform. [Drop] is unique in that it
        generates two DOM events, ondrop and ondragend, in that order. *)
    type action =
      | Drag of string (** Begins dragging a DOM item specified via a CSS selector *)
      | Enter of string (** Moves a previously dragged item above a drop target *)
      | Leave
      (** Moves a previously dragged item out of a previously entered drop target *)
      | Drop (** Drops a previously dragged item onto a previously entered drop target *)

    (** The main driver function for drag-and-drop tests. Call this function to
        see the effect of a sequence of drag-and-drop actions. An exception
        will be raise when the sequence of actions is not logical (for example,
        entering a drop zone before anything has begun being dragged).

        The drag-and-drop assumptions about a frame happening between
        [dragenter] and [drop] is automatically satisfied by this library, via
        a extra calls to [recompute_view] to trigger lifecycle handlers.
    *)
    val run
      :  ('a, 'b) handle
      -> get_vdom:('a -> Virtual_dom.Vdom.Node.t)
      -> t
      -> action list
      -> t
  end

end
