open! Core
open! Bonsai_web

(** [B]ounding Box. *)
module Bbox : sig
  type 'a t =
    { min_x : 'a
    ; max_x : 'a
    ; min_y : 'a
    ; max_y : 'a
    }
  [@@deriving sexp, equal]

  (* expanded int and float boxes are provided so that they can be easily used as
     first-class modules for [Bonsai.state] *)
  module Int : sig
    type nonrec t = int t [@@deriving sexp, equal]

    val width : t -> int
    val height : t -> int
  end

  module Float : sig
    type nonrec t = float t [@@deriving sexp, equal]

    val width : t -> float
    val height : t -> float
  end
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
  :  ?client_rect_changed:(Bbox.Int.t -> unit Effect.t)
  -> ?visible_rect_changed:(Bbox.Int.t option -> unit Effect.t)
  -> unit
  -> Vdom.Attr.t

module For_testing : sig
  type t =
    { client_rect_changed : int Bbox.t -> unit Effect.t
    ; visible_rect_changed : int Bbox.t option -> unit Effect.t
    }

  val type_id : t Type_equal.Id.t
  val intersect_and_reproject : float Bbox.t -> float Bbox.t -> int Bbox.t option

  val compute_visibility
    :  client_bbox:float Bbox.t
    -> window_bbox:float Bbox.t
    -> parents:float Bbox.t list
    -> int Bbox.t option
end
