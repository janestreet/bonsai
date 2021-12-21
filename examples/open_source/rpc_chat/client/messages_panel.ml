open! Core
open! Bonsai_web
open Bonsai_chat_open_source_common
open Bonsai.Let_syntax

let view_message { Message.room = _; author; contents } =
  Vdom.Node.div [ Vdom.Node.textf "%s: %s" author contents ]
;;

let view messages current_room =
  Vdom.Node.div
    ~attr:(Vdom.Attr.id "messages-list")
    [ Vdom.Node.h1 [ Vdom.Node.text (Room.to_string current_room) ]
    ; Vdom.Node.div (List.map messages ~f:view_message)
    ]
;;

let component ~messages ~current_room =
  let%arr messages = messages
  and current_room = current_room in
  view messages current_room
;;
