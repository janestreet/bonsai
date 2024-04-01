open! Core
open! Bonsai_web.Cont
open Bonsai_chat_open_source_common

let component
  ~room_list
  ~current_room
  ~messages
  ~refresh_rooms
  ~change_room
  ~send_message
  graph
  =
  let open Bonsai.Let_syntax in
  let send_message =
    match%arr current_room with
    | Some room -> fun contents -> send_message ~room ~contents
    | None -> Fn.const Effect.never
  in
  let current_room =
    current_room >>| Option.value ~default:(Room.of_string "no room selected")
  in
  let rooms_list =
    Room_list_panel.component ~room_list ~refresh_rooms ~change_room graph
  in
  let compose_panel = Compose_message.component ~send_message graph in
  let messages_panel = Messages_panel.component ~messages ~current_room graph in
  let%arr rooms_list = rooms_list
  and compose_panel = compose_panel
  and messages_panel = messages_panel in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "container" ]
    [ rooms_list
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.id "message-container" ]
        [ messages_panel; compose_panel ]
    ]
;;
