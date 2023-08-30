open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Knobs = Knobs

module Style =
[%css
stylesheet
  {|
  :root, body, .container {
    margin: 0;
    padding: 0;
  }

  .container {
    width: 100%;
  }

  body {
    padding: 1em;
  }

  .box {
    flex-grow: 1;
    height: 100px;
  }

  .gradient-container,
  .overlay-container {
    border-radius: 5px;
    overflow:hidden;
  }

  .overlay-container {
      background: repeating-conic-gradient(black 0% 25%, white 0% 50%) 50% / 20px 20px;
  }
|}]

let box_with_color ?(content = Vdom.Node.none) color =
  let css_color =
    color
    |> Oklab.to_string_css
    |> (fun h -> `Hex h)
    |> Css_gen.background_color
    |> Vdom.Attr.style
  in
  Vdom.Node.div ~attrs:[ Style.box; css_color ] [ content ]
;;

let gradient ~left ~right ~steps =
  List.init (steps + 1) ~f:(fun i ->
    let mult = Float.of_int i /. Float.of_int steps in
    let color = Oklab.lerp left right mult in
    box_with_color color)
;;

let overlay ~left ~right =
  [ box_with_color left
  ; box_with_color (Oklab.composite ~under:left ~over:right)
  ; box_with_color right
  ]
;;

let component =
  let%sub form = Knobs.form in
  let%arr ( { shared = { left = `Hex left; right = `Hex right }
            ; for_gradient = { steps }
            ; for_overlay = { left_alpha; right_alpha }
            }
          , view )
    =
    form
  in
  let left = Oklab.of_rgb_hex left in
  let right = Oklab.of_rgb_hex right in
  let gradient = gradient ~left ~right ~steps in
  let overlay =
    let left = Oklab.set_alpha left ~alpha:left_alpha in
    let right = Oklab.set_alpha right ~alpha:right_alpha in
    overlay ~left ~right
  in
  View.vbox
    ~gap:(`Em 1)
    [ Vdom.Node.div [ view ]
    ; View.hbox ~attrs:[ Style.gradient_container ] gradient
    ; View.hbox ~attrs:[ Style.overlay_container ] overlay
    ]
;;

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app (Value.return (Kado.theme ~version:Bleeding ())) component)
;;
