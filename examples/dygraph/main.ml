open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let app graph =
  let hide_overnight = Hide_overnights.app graph in
  let simple = Simple.app graph in
  let stock_chart = Stock_chart.app graph in
  let custom_points = Custom_points.app graph in
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

let () = Bonsai_web.Start.start app
