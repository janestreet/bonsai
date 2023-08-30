open! Core
open! Bonsai_web
open! Js_of_ocaml

module Behavior : sig
  type t =
    | Grow_to_fill
    | Shrink_to_avoid_overflow
    | Grow_or_shrink_to_match_parent_size
end

(** This hook is placed on a node, and that node will be resized
    to match the content-size of its parent.  The different behaviors
    are as follows:

    - Grow_to_fill: The element will only grow to fill space. never shrink.
    - Shrink_to_avoid_overflow: The element will only shrink to avoid clipping
      out of its parents content.  It will never grow.
    - Grow_or_shrink_to_match_parent_size: The node will grow or shrink to take
      up as much space as possible.

    The default behavior is [Shrink_to_avoid_overflow] *)
val attr : ?behavior:Behavior.t -> unit -> Vdom.Attr.t

(** It is recommended that you place these attributes on the parent of the
    node that is given [attr].

    Currently all this attribute does is set [overflow: hidden], which prevents
    scrollbars from showing up on the parent node. *)
val attr_for_parent__recommended : Vdom.Attr.t
