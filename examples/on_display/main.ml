open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub state, set_state =
    Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
  in
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
