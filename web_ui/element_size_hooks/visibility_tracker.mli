open! Core
open! Bonsai_web

module Bounds : sig
  type t =
    { min_x : int
    ; min_y : int
    ; max_x : int
    ; max_y : int
    }
  [@@deriving sexp, equal]
end

(** When the attribute returned from this function is attached to a dom node,
    the attribute tracks the visible region of the node in the browser.

    E.g. when the node is visible fully, min_x = min_y = 0, and max_x = node's width,
    max_y = node's height.

    The bounds is conservative; it may over-estimate the size, but will never
    underestimate the size.  Over-estimation can occur when a parent of the
    tracked node has a more restrictive size than the tracked node such that a
    scrollbar is present.

    This is mostly useful for large nodes, to implement some custom scrolling
    behaviour - if your browser has height 500px, and you have a node with
    height 1000px, the y bounds will be (0, 500) if scrolled to the top (so
    that the top of the node visible), than (50, 550) as you scroll 50 pixels
    down, etc. *)
val on_change : (Bounds.t -> unit Ui_effect.t) -> Vdom.Attr.t

module For_testing : sig
  val type_id : (Bounds.t -> unit Ui_effect.t) Type_equal.Id.t
end
