open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub is_connected, set_is_connected =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub not_connected_warning_box =
    Bonsai_web_ui_not_connected_warning_box.(
      component ~create_message:message_for_async_durable is_connected)
  in
  let%arr not_connected_warning_box = not_connected_warning_box
  and is_connected = is_connected
  and set_is_connected = set_is_connected in
  Vdom.Node.div
    [ not_connected_warning_box
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_is_connected (not is_connected)) ]
        [ Vdom.Node.text "toggle is_connected" ]
    ; Vdom.Node.text
        "This button simulates connecting and disconnecting from the server. This \
         example does not actually affect the connection, since it's sole purpose is to \
         demonstrate the appearance of the warning box that is displayed at the \
         bottom-right of the page"
    ]
;;

let () = Bonsai_web.Start.start component
