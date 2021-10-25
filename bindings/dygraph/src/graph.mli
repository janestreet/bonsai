open! Core
open! Import
open  Gen_js_api

(** API: http://dygraphs.com/jsdoc/symbols/Dygraph.html *)
type t

val create        : Native_node.t -> Data.t -> Options.t -> t [@@js.new "Dygraph"]
val destroy       : t -> unit [@@js.call]
val resize        : t -> unit [@@js.call]
val updateOptions : t -> Update_options.t -> unit [@@js.call]
val getArea       : t -> Area.t [@@js.call]
val isZoomed      : t -> bool [@@js.call]
val resetZoom     : t -> unit [@@js.call]
val t_to_js       : t -> Ojs.t
val t_of_js       : Ojs.t -> t
