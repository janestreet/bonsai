open! Core
open! Bonsai_web

(** [B]ounding Box. *)
module Bbox : sig
  type t =
    { min_x : float
    ; max_x : float
    ; min_y : float
    ; max_y : float
    }
  [@@deriving sexp, equal]

  val width : t -> float
  val height : t -> float
end

(** This attribute tracks two computed properties of the size, position, and
    layout of the element that it is attached to:

    - The client-rect of the element
      https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect

    - A bounding-rect for the portion of the element that is visible (by factoring in
      the bounding-rects of parents), reprojected into the coordinate space of the
      target element.

    The bounds are precise, but won't factor in any absolutely, relatively, or stickily
    positioned elements that may still occlude the element. *)
val detect
  :  ?client_rect_changed:(Bbox.t -> unit Effect.t)
  -> ?visible_rect_changed:(Bbox.t option -> unit Effect.t)
  -> unit
  -> Vdom.Attr.t

module For_testing : sig
  type t =
    { client_rect_changed : Bbox.t -> unit Effect.t
    ; visible_rect_changed : Bbox.t option -> unit Effect.t
    }

  val type_id : t Type_equal.Id.t
end
