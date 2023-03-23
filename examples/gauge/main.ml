open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Gauge = Bonsai_web_ui_gauge
open Vdom

module Styles =
  [%css
    stylesheet
      {|
html {
  font-family: "Open Sans", "Noto Color Emoji", sans-serif;
}
.paper {
  box-shadow: 0 0 8px rgba(0,0,0,0.2);
  padding: 12px;
  border-radius: 4px;
  margin: 8px;
  max-width: fit-content;
  text-align: center;
}

.row {
  display: flex;
  flex-direction: row;
  align-items: center;
  align-content: center;
}

.column {
  display: flex;
  flex-direction: column;
  align-items: center;
  align-content: center;
}

pre {
  font-family: 'Courier New', monospace;
  background-color: #FCFCFC;
  border: 1px solid #E7E7E7;
  text-align: left;
  margin: 4px;
  padding: 4px;
  max-height: 50vh;
  overflow-y: auto;
}
                          |}]

let colors =
  [| "#ff355e"
   ; "#fd5b78"
   ; "#ff6037"
   ; "#ff9966"
   ; "#ff9933"
   ; "#ffcc33"
   ; "#ffff66"
   ; "#ccff00"
   ; "#66ff66"
   ; "#aaf0d1"
   ; "#50bfe6"
   ; "#ff6eff"
   ; "#ee34d2"
   ; "#ff00cc"
  |]
;;

let radius = 30.

let ticker =
  let%sub percentage, increase =
    Bonsai.state_machine0
      (module Int)
      (module Unit)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> (model + 1) % 101)
  in
  let%sub color_index, increment =
    Bonsai.state_machine0
      (module Int)
      (module Unit)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ prev () ->
        prev + (1 % Array.length colors))
  in
  let%sub () =
    let%sub effect =
      let%arr increase = increase
      and increment = increment in
      Effect.Many [ increase (); increment () ]
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:false
      (Time_ns.Span.of_sec 0.1)
      effect
  in
  let%arr percentage = percentage
  and color_index = color_index in
  Percent.of_percentage (Int.to_float percentage), color_index
;;

let component =
  let%sub percentage, color_index = ticker in
  let%sub gauge1 =
    let%arr percentage = percentage in
    Gauge.create ~radius percentage
  in
  let%sub gauge2 =
    let percent_to_color p =
      let open Float in
      let p = Percent.to_percentage p in
      if p < 30.0
      then Tailwind_colors.amber600
      else if p < 60.0
      then Tailwind_colors.amber500
      else if p < 90.0
      then Tailwind_colors.red600
      else Tailwind_colors.red500
    in
    let%arr percentage = percentage in
    Gauge.create ~percent_to_color ~radius percentage
  in
  let%sub gauge3 =
    let%sub percent_to_color =
      let%arr color_index = color_index in
      let color = Array.get colors (color_index % Array.length colors) in
      Fn.const (`Hex color)
    in
    let%arr percent_to_color = percent_to_color
    and percentage = percentage in
    Gauge.create ~percent_to_color ~radius percentage
  in
  let%arr gauge1 = gauge1
  and gauge2 = gauge2
  and gauge3 = gauge3 in
  Node.div
    ~attrs:[ Styles.column ]
    [ Node.strong [ Node.text "Gauges" ]
    ; Node.div
        ~attrs:[ Styles.row ]
        [ Node.div ~attrs:[ Styles.paper ] [ gauge1 ]
        ; Node.div ~attrs:[ Styles.paper ] [ gauge2 ]
        ; Node.div ~attrs:[ Styles.paper ] [ gauge3 ]
        ]
    ]
;;

let () = Bonsai_web.Start.start component
