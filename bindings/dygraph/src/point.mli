open! Core
open! Import
open Gen_js_api

(** http://dygraphs.com/options.html#point_properties *)

type t =
  { xval : float
  ; yval : float
  ; canvasx : float
  ; canvasy : float
  ; name : string
  ; idx : int
  }

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
