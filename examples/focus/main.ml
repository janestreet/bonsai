open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let component =
  let%sub focus_on_activate_attr = Effect.Focus.on_activate () in
  let%sub focus_on_effect = Effect.Focus.on_effect () in
  let%sub theme = View.Theme.current in
  let%arr focus_on_activate_attr = focus_on_activate_attr
  and focus_on_effect_attr, effect_to_focus = focus_on_effect
  and theme = theme in
  View.hbox
    [ Vdom.Node.input ~attrs:[ focus_on_activate_attr ] ()
    ; Vdom.Node.input ~attrs:[ focus_on_effect_attr ] ()
    ; View.button theme "click to focus second input" ~on_click:effect_to_focus
    ]
;;

let () = Bonsai_web.Start.start component
