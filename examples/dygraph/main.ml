open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let app =
  let%sub hide_overnight = Hide_overnights.app in
  let%sub simple = Simple.app in
  let%sub stock_chart = Stock_chart.app in
  return
  @@ let%map hide_overnight = hide_overnight
  and simple = simple
  and stock_chart = stock_chart in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Simple" ]
    ; simple
    ; Vdom.Node.h1 [ Vdom.Node.text "Stock" ]
    ; stock_chart
    ; Vdom.Node.h1 [ Vdom.Node.text "Hide Overnight" ]
    ; hide_overnight
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
