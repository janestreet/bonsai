open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

module Style =
  [%css.raw
    {|
   .compose {
     display: flex;
     flex-direction:row;
   }

   .compose button {
      width:40px;
   }

   .compose input {
      flex: 1;
   }
|}]

let component ~send_message =
  let%sub textbox_state = Bonsai.state [%here] (module String) ~default_model:"" in
  let%arr textbox_content, set_textbox_content = textbox_state
  and send_message = send_message in
  let submit_and_then_clear () =
    Effect.Many [ send_message textbox_content; set_textbox_content "" ]
  in
  let on_ret =
    Vdom.Attr.on_keypress (fun key ->
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event key with
      | Enter -> submit_and_then_clear ()
      | _ -> Effect.Ignore)
  in
  let on_input = Vdom.Attr.on_input (fun _ -> set_textbox_content) in
  let value = Vdom.Attr.value_prop textbox_content in
  let text_input =
    Vdom.Node.input
      ~attr:(Vdom.Attr.many [ on_ret; on_input; value ])
      [ Vdom.Node.text "submit" ]
  in
  let submit_button =
    Vdom_input_widgets.Button.simple "send" ~on_click:submit_and_then_clear
  in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.compose) [ text_input; submit_button ]
;;
