open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let component graph =
  let focus_on_activate_attr = Effect.Focus.on_activate () graph in
  let focus_on_effect = Effect.Focus.on_effect () graph in
  let theme = View.Theme.current graph in
  let%arr focus_on_activate_attr = focus_on_activate_attr
  and { attr = focus_on_effect_attr; focus = effect_to_focus; blur = _ } = focus_on_effect
  and theme = theme in
  View.hbox
    [ Vdom.Node.input ~attrs:[ focus_on_activate_attr ] ()
    ; Vdom.Node.input ~attrs:[ focus_on_effect_attr ] ()
    ; View.button theme "click to focus second input" ~on_click:effect_to_focus
    ]
;;

let () = Bonsai_web.Start.start component
