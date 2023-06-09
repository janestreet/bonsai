open! Core
open Bonsai_web

(** To use this API, first call [create] to make a "universe" where
    drag-and-drop can happen. It is possible, and sometimes desireable to
    create multiple universes; for example, if two parts of an interface
    support drag-and-drop, but they are not meant to interact, each should
    probably have their own universe.

    Each universe provides a [source] attribute for designating nodes as
    draggable (it is named "source" rather than "draggable" because "draggable"
    is already the name of an HTML attribute), a [drop_target] attribute for
    designating nodes as droppable areas, and a [model] which exposes the
    current state of the drag-and-drop process so that the UI can provide good
    visual feedback.

    The inputs to both [source] and [drop_target] get reflected in the [model].
    For example, in a re-orderable list, the [source] could be an identifier
    pointing to the content being dragged, and the [drop_target] could be the
    position in the list that the target node corresponds to. Thus, when a node
    is dragged to hover over another node, it will be clear to the UI which
    items should receive visual flare to indicate that a drop is impending.

    A formality in using this library is placing the [sentinel] attribute
    somewhere in the DOM. Doing so makes the state of the universe available to
    be directly affected in tests. Without a [sentinel], the app will continue
    to work, but you will be unable to test drag-and-drop interactions.

    There is nothing preventing a node from having two [drop_target] or two
    [source] attributes - such a node resides in both the universes universes.
    Dragging a source that is inside two universes onto a target will trigger
    the [on_drop] callbacks for one universe and cancel the dragging for the
    other one. Likewise, when dragging a source onto a target that resides in
    multiple universes, only the universe that the source is from will trigger
    it's [on_drop]. In other words, putting a node in multiple universes does
    the most obvious thing. *)

module Model : sig
  type ('source_id, 'target_id) t =
    | Not_dragging
    | Dragging of
        { source : 'source_id
        ; target : 'target_id option
        }
  [@@deriving sexp, equal]
end

(** Contains everything needed to build drag-and-drop components. Each value of
    this type represents its own universe of drag-and-drop attributes. Nodes
    which have the [source] attribute from one universe cannot be dragged on
    top of [drop_target]s from another universe. *)
type ('source_id, 'target_id) t

(** Turns a [('s, 'a) t]  into a [('s, 'b) t]. (Changes the type of your drag target. )*)
val project_target
  :  ('source, 'target_a) t
  -> map:('target_a -> 'target_b)
  -> unmap:('target_b -> 'target_a)
  -> ('source, 'target_b) t

(** A node with the [source] attribute will set its universe's currently
    dragged value to the input ['source_id]. *)
val source : ('source_id, 'target_id) t -> id:'source_id -> Vdom.Attr.t


(** While this attribute does nothing while the app is running, it facilitates
    testing by allowing tests to directly change the state of the universe. *)
val sentinel : ('source_id, 'target_id) t -> name:string -> Vdom.Attr.t

(** Designates a "drop zone" node in a particular universe *)
val drop_target : ('source_id, 'target_id) t -> id:'target_id -> Vdom.Attr.t

(** Extracts the current state of the universe. This is useful for providing
    visual feedback about what will happen if an item is dropped *)
val model : ('source_id, 'target_id) t -> ('source_id, 'target_id) Model.t

module type S = sig
  type t [@@deriving sexp, equal]
end

(** Creates a new drag-and-drop universe. *)
val create
  :  source_id:(module S with type t = 'source_id)
  -> target_id:(module S with type t = 'target_id)
  -> on_drop:('source_id -> 'target_id -> unit Effect.t) Value.t
  -> ('source_id, 'target_id) t Computation.t

(** A node which is follows the mouse when something is being dragged, but is
    invisible otherwise. The result of [f] is wrapped in a [div] which has the
    same dimensions as the element which was originally dragged, allowing the
    drag operation to look visually seamless. *)
val dragged_element
  :  ('source_id, _) t Value.t
  -> f:('source_id Value.t -> Vdom.Node.t Computation.t)
  -> Vdom.Node.t Computation.t

module For_testing : sig
  (** Actions for testing use strings instead of the same types in the
      universe because then the tests do not need a [t], but can
      refer to an existing universe just by their name. *)
  module Action : sig
    type t =
      | Start_drag of string
      | Set_target of string option
      | Finish_drag
    [@@deriving sexp, equal]
  end

  val type_id : (Action.t -> unit Effect.t) Type_equal.Id.t
end
