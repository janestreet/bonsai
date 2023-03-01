open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub state, set_state = Bonsai.state (module Int) ~default_model:0 in
  let%sub increment =
    let%arr state = state
    and set_state = set_state in
    set_state (state + 1)
  in
  let%sub () = Bonsai.Edge.after_display increment in
  let%arr state = state in
  Vdom.Node.textf "Number of frames rendered: %d" state
;;

let () = Bonsai_web.Start.start component
