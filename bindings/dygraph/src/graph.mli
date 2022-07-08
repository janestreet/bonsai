open! Core
open! Import
open  Gen_js_api

(** API: http://dygraphs.com/jsdoc/symbols/Dygraph.html *)
type t

val create          : Native_node.t -> Data.t -> Options.t -> t [@@js.new "Dygraph"]
val destroy         : t -> unit [@@js.call]
val resize          : t -> unit [@@js.call]
val resize_explicit : t -> width:int -> height:int -> unit [@@js.call "resize"]
val updateOptions   : t -> Update_options.t -> unit [@@js.call]
val getArea         : t -> Area.t [@@js.call]
val isZoomed        : t -> bool [@@js.call]
val resetZoom       : t -> unit [@@js.call]

(** Dygraph supports multiple axes which is why it takes an [int] for the [axis]. *)
val toDomCoords     : ?axis:[ `X | `Y ] -> x:float -> y:float -> t -> float array
[@@js.custom
  let toDomCoords ?(axis = `X) ~(x : float) ~(y : float) (t : t) =
    Ojs.array_of_js
      Ojs.float_of_js
      (Ojs.call
         (t_to_js t)
         "toDomCoords"
         [| Ojs.float_to_js x
          ; Ojs.float_to_js y
          ; Ojs.int_to_js
              (match axis with
               | `X -> 0
               | `Y -> 1)
         |])
  ;;]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
