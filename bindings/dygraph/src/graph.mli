open! Core
open! Import
open Gen_js_api

(** API: http://dygraphs.com/jsdoc/symbols/Dygraph.html *)
type t

val create : Native_node.t -> Data.t -> Options.t -> t [@@js.new "Dygraph"]
val destroy : t -> unit [@@js.call]
val resize : t -> unit [@@js.call]
val resize_explicit : t -> width:int -> height:int -> unit [@@js.call "resize"]
val updateOptions : t -> Update_options.t -> unit [@@js.call]
val getArea : t -> Area.t [@@js.call]
val isZoomed : t -> bool [@@js.call]
val resetZoom : t -> unit [@@js.call]

(** [primary_context] is the rendering context to which Dygraphs draws data points and
    axes and underlays -- everything except the interactive highlights drawn to
    [overlay_context]. *)
val primary_context : t -> Canvas_rendering_context_2D.t
  [@@js.get "hidden_ctx_"]

(** [overlay_context] is the rendering context for temporary overlays that change
    interactively, such as the highlighted point on mouse hover or the selected zoom
    region during a drag event. The separate canvas allows Dygraphs to quickly clear and
    redraw those overlays without having to re-render the entire graph. *)
val overlay_context : t -> Canvas_rendering_context_2D.t
  [@@js.get "canvas_ctx_"]

(** Despite the name of the function, this returns a point in the coordinate space of the
    graph's canvas element. This function is suitable for finding drawing coordinates when
    rendering a custom overlay or underlay.

    Unlike when using Dygraph annotations, the point that you pass in does not actually
    have to exist in any data series on the graph. But if you are using a multi-series
    graph with multiple Y axes, make sure that you pass the correct [axis]. *)
val toDomCoords : ?axis:Which_y_axis.t -> x:float -> y:float -> t -> float * float
  [@@js.custom
    let toDomCoords ?(axis = `y1) ~(x : float) ~(y : float) (t : t) =
      let coords =
        Ojs.call
          (t_to_js t)
          "toDomCoords"
          [| Ojs.float_to_js x
           ; Ojs.float_to_js y
           ; Ojs.int_to_js
               (match axis with
                | `y1 -> 0
                | `y2 -> 1)
          |]
      in
      Ojs.float_of_js (Ojs.array_get coords 0), Ojs.float_of_js (Ojs.array_get coords 1)
    ;;]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
