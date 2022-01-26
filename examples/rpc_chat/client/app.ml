open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_chat_common

module Style =
  [%css.raw
    {|
  html, body, .container {
      height: 100vh;
      margin:0;
      padding:0;
      width: 100vw;
  }

  .container {
      max-width:800px;
      width:50vw;
      margin:0 auto;

      display:flex;
      flex-direction:row;
      flex-basis:100%;
      flex: 1;
  }

  .message_container {
      display:flex;
      flex-direction:column;
      flex:1;
      padding:1em;
      padding-top:0;
  }
|}]

let component ~room_list ~current_room ~messages ~refresh_rooms ~change_room ~send_message
  =
  let send_message =
    match%map current_room with
    | Some room -> fun contents -> send_message ~room ~contents
    | None -> fun _ -> Effect.never
  in
  let current_room =
    current_room >>| Option.value ~default:(Room.of_string "no room selected")
  in
  let%sub rooms_list = Room_list_panel.component ~room_list ~refresh_rooms ~change_room in
  let%sub compose_panel = Compose_message.component ~send_message in
  let%sub messages_panel = Messages_panel.component ~messages ~current_room in
  let%arr rooms_list = rooms_list
  and compose_panel = compose_panel
  and messages_panel = messages_panel in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.container)
    [ rooms_list
    ; Vdom.Node.div
        ~attr:(Vdom.Attr.class_ Style.message_container)
        [ messages_panel; compose_panel ]
    ]
;;
