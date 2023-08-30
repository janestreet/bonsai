open! Core
open! Import
open Gen_js_api

(** An object with {x,y,w,h} properties describing the drawing area, for use in
    [underlayCallback].  All units are in pixels (I think). *)

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
