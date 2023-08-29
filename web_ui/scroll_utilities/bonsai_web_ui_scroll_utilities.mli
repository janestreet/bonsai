open! Core
open! Bonsai_web

(** Scrolls to a position inside an element found by the provided selector.
    The target element must be relatively or absolutely positioned.

    Returns a [unit Or_error.t Effect.t] that will be an error if the
    element picked by "selector" can't be found.

    `Minimal, `To_bottom, and `To_top are used to determine where the
    chosen pixel would end up inside the scrolling container after the
    scrolling is finished. *)
val to_position_inside_element
  :  ?smooth:bool
  -> selector:string
  -> x_px:float
  -> y_px:float
  -> [ `Minimal | `To_bottom | `To_top ]
  -> unit Or_error.t Effect.t

(** Similar to the above function, but instead of scrolling to a position,
    it scrolls to a specific element *)
val into_view
  :  ?smooth:bool
  -> selector:string
  -> [ `Minimal | `To_top | `To_bottom ]
  -> unit Or_error.t Effect.t
