open! Core
open Js_of_ocaml

let cancel_animation_frame id = Dom_html.window##cancelAnimationFrame id

let request_animation_frame f =
  Dom_html.window##requestAnimationFrame (Js.wrap_callback f)
;;

let add_event_listener ?(use_capture = false) ?(prevent_default = false) element event ~f =
  Dom_html.addEventListener
    element
    event
    (Dom.full_handler (fun element event ->
       f element event;
       Js.bool prevent_default))
    (Js.bool use_capture)
;;

let set_width element x = element##.style##.width := Js.string (sprintf "%fpx" x)
let set_height element x = element##.style##.height := Js.string (sprintf "%fpx" x)
let reset_width element = element##.style##.width := Js.string ""
let reset_height element = element##.style##.height := Js.string ""
