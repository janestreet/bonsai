open! Core
open! Import
open! Gen_js_api

type t

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
val line_plotter : t [@@js.global "Dygraph.Plotters.linePlotter"]
val fill_plotter : t [@@js.global "Dygraph.Plotters.fillPlotter"]
val error_bar_plotter : t [@@js.global "Dygraph.Plotters.errorPlotter"]
val point_plotter : t [@@js.global "Dygraph.Plotters.pointPlotter"]
