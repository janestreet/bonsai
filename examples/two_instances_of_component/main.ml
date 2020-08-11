open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

let counter =
  let%sub state = Bonsai.state [%here] (module Int) ~default_model:0 in
  return
  @@ let%map current_value, set_value = state in
  Vdom.Node.div
    []
    [ Vdom.Node.textf "%d" current_value
    ; Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ -> set_value (current_value + 1)) ]
        [ Vdom.Node.text "increment" ]
    ; Vdom.Node.button
        [ Vdom.Attr.on_click (fun _ -> set_value (current_value - 1)) ]
        [ Vdom.Node.text "decrement" ]
    ]
;;

let two_counters =
  let open Bonsai.Let_syntax in
  let%sub counter_1 = counter in
  let%sub counter_2 = counter in
  return
  @@ let%map counter_1 = counter_1
  and counter_2 = counter_2 in
  Vdom.Node.div [] [ counter_1; counter_2 ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" two_counters
;;
