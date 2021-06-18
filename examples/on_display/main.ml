open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub state, set_state = Bonsai.state [%here] (module Int) ~default_model:0 in
  let increment =
    let%map state = state
    and set_state = set_state in
    set_state (state + 1)
  in
  let%sub () = Bonsai.Edge.after_display increment in
  return
  @@ let%map state = state in
  Vdom.Node.textf "Number of frames rendered: %d" state
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
