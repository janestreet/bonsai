open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

let view_message { Message.room = _; author; contents } =
  Vdom.Node.div [] [ Vdom.Node.textf "%s: %s" author contents ]
;;

let view messages current_room =
  Vdom.Node.div
    [ Vdom.Attr.id "messages-list" ]
    [ Vdom.Node.h1 [] [ Vdom.Node.text (Room.to_string current_room) ]
    ; Vdom.Node.div [] (List.map messages ~f:view_message)
    ]
;;

let component ~messages ~current_room =
  let open Bonsai.Let_syntax in
  Bonsai.read (view <$> messages <*> current_room)
;;
