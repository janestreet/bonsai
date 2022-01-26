open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_chat_common

module Style = [%css.raw {|
  .messages_list {
    flex: 1;
  }
|}]

let view_message { Message.room = _; author; contents } =
  Vdom.Node.div [ Vdom.Node.textf "%s: %s" author contents ]
;;

let component ~messages ~current_room =
  let%arr messages = messages
  and current_room = current_room in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.messages_list)
    [ Vdom.Node.h1 [ Vdom.Node.text (Room.to_string current_room) ]
    ; Vdom.Node.div (List.map messages ~f:view_message)
    ]
;;
