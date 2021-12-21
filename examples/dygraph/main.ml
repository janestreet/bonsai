open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let app =
  let%sub hide_overnight = Hide_overnights.app in
  let%sub simple = Simple.app in
  let%sub stock_chart = Stock_chart.app in
  let%sub custom_points = Custom_points.app in
  let%arr hide_overnight = hide_overnight
  and simple = simple
  and stock_chart = stock_chart
  and custom_points = custom_points in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Simple" ]
    ; simple
    ; Vdom.Node.h1 [ Vdom.Node.text "Stock" ]
    ; stock_chart
    ; Vdom.Node.h1 [ Vdom.Node.text "Hide Overnight" ]
    ; hide_overnight
    ; Vdom.Node.h1 [ Vdom.Node.text "Custom points" ]
    ; custom_points
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
