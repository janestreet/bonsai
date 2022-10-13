open! Core
open! Bonsai_web

(** [only_when_visible] runs the provided computation for at least one frame
    initially, and then if the node is visible, will keep that computation active,
    but if it ever becomes hidden, the computation is deactivated and the most
    recent computed vdom node is returned instead.  Upon becoming visible again,
    the computation is reactivated.

    [visible_attr] and [hidden_attr] are optional attributes that will be attached
    to the dom nodes based on their visibility status. *)
val only_when_visible
  :  ?visible_attr:Vdom.Attr.t Value.t
  -> ?hidden_attr:Vdom.Attr.t Value.t
  -> Vdom.Node.t Computation.t
  -> Vdom.Node.t Computation.t

(** Like [only_when_visible], but if the component returns more than just a vdom node,
    you can propagate the value to the outside.  This value is [Some] when the computation
    is active, and otherwise [None]. *)
val only_when_visible'
  :  ?visible_attr:Vdom.Attr.t Value.t
  -> ?hidden_attr:Vdom.Attr.t Value.t
  -> (Vdom.Node.t * 'a) Computation.t
  -> (Vdom.Node.t * 'a option) Computation.t

module Tracker : sig
  (** A lower-level API, [Tracker.component] returns an attribute that can be attached to
      a vdom node along with the status for the visibility of the node that the attribute
      was attached to. *)

  type t =
    | Visible
    | Hidden
    | Unknown

  val component : (t * Vdom.Attr.t) Computation.t
end
