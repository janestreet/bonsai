open! Core
open! Bonsai_web
open Bonsai_chat_open_source_common

let component ~room_list ~current_room ~messages ~refresh_rooms ~change_room ~send_message
  =
  let open Bonsai.Let_syntax in
  let%sub send_message =
    match%arr current_room with
    | Some room -> fun contents -> send_message ~room ~contents
    | None -> Fn.const Effect.never
  in
  let%sub current_room =
    return (current_room >>| Option.value ~default:(Room.of_string "no room selected"))
  in
  let%sub rooms_list = Room_list_panel.component ~room_list ~refresh_rooms ~change_room in
  let%sub compose_panel = Compose_message.component ~send_message in
  let%sub messages_panel = Messages_panel.component ~messages ~current_room in
  let%arr rooms_list = rooms_list
  and compose_panel = compose_panel
  and messages_panel = messages_panel in
  Vdom.Node.div
    ~attr:(Vdom.Attr.id "container")
    [ rooms_list
    ; Vdom.Node.div
        ~attr:(Vdom.Attr.id "message-container")
        [ messages_panel; compose_panel ]
    ]
;;
