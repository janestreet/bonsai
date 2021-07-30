(** [Freeze.width/height] looks at the size of the node when it enters the dom,
    discovers its calculated width/height, and then sets it explicitly (in px).
    This prevents the browser layout from messing with a size that we want to be
    controlled by the user. *)

open! Core
open! Bonsai_web
open! Import
open Js_of_ocaml

val width : Vdom.Attr.t
val height : Vdom.Attr.t

module Expert : sig
  val set_width : Dom_html.element Js.t -> unit
  val set_height : Dom_html.element Js.t -> unit
  val reset_width : Dom_html.element Js.t -> unit
  val reset_height : Dom_html.element Js.t -> unit
end
