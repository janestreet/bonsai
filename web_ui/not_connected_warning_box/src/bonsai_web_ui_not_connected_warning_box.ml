open! Core
open Virtual_dom
open Bonsai.Let_syntax

let message_for_async_durable time_span =
  sprintf
    "You've been disconnected from the server for %s. There is no need to refresh the \
     page, since the web client will reconnect automatically when the server becomes \
     available again."
    (Time_ns.Span.to_string_hum ~decimals:0 time_span)
;;

let component ?(styles = Vdom.Attr.empty) ~create_message is_connected =
  if%sub is_connected
  then Bonsai.const (Vdom.Node.div ~attr:(Vdom.Attr.style (Css_gen.display `None)) [])
  else (
    let%sub activation_time, set_activation_time =
      Bonsai.state_opt [%here] (module Time_ns.Alternate_sexp)
    in
    let%sub now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:(set_activation_time <*> (now >>| Option.some))
        ()
    in
    return
      (let%map now = now
       and activation_time = activation_time in
       let duration_of_visibility =
         Time_ns.diff now (Option.value ~default:now activation_time)
       in
       Vdom.Node.div
         ~attr:styles
         [ Vdom.Node.div
             ~attr:(Vdom.Attr.style Css_gen.(font_size (`Rem 1.5) @> font_weight `Bold))
             [ Vdom.Node.text "Warning!" ]
         ; Vdom.Node.div [ Vdom.Node.text (create_message duration_of_visibility) ]
         ]))
;;
