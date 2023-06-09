open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_chat_common

module Style =
  [%css
    stylesheet
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

let component ~current_room ~messages ~change_room ~obfuscate_message =
  let%sub dispatch_room_list =
    Rpc_effect.Rpc.dispatcher Protocol.List_rooms.t ~where_to_connect:Self
  in
  let%sub fetch_room_list =
    let%arr dispatch_room_list = dispatch_room_list in
    match%map.Effect dispatch_room_list () with
    | Ok rooms -> rooms
    | Error _ -> []
  in
  let%sub room_list, refresh_rooms =
    Bonsai.Edge.Poll.manual_refresh
      ~sexp_of_model:[%sexp_of: Room.t list]
      ~equal:[%equal: Room.t list]
      (Bonsai.Edge.Poll.Starting.initial [])
      ~effect:fetch_room_list
  in
  let%sub dispatch_send_message =
    Rpc_effect.Rpc.dispatcher Protocol.Send_message.t ~where_to_connect:Self
  in
  let%sub send_message =
    let%arr dispatch_send_message = dispatch_send_message
    and current_room = current_room in
    match current_room with
    | Some room ->
      fun contents ->
        let%map.Effect (_ : unit Or_error.t Or_error.t) =
          dispatch_send_message
            { Message.room; contents = obfuscate_message contents; author = "" }
        in
        ()
    | None -> fun _ -> Effect.never
  in
  let current_room =
    current_room >>| Option.value ~default:(Room.of_string "no room selected")
  in
  let%sub rooms_list = Room_list_panel.component ~room_list ~refresh_rooms ~change_room in
  let%sub compose_panel = Compose_message.component ~send_message in
  let%sub messages_panel = Messages_panel.component ~messages ~current_room in
  let%sub connection_status = Rpc_effect.Status.state ~where_to_connect:Self in
  let%arr rooms_list = rooms_list
  and compose_panel = compose_panel
  and messages_panel = messages_panel
  and connection_status = connection_status in
  Vdom.Node.div
    ~attrs:[ Style.container ]
    [ rooms_list
    ; Vdom.Node.div
        ~attrs:[ Style.message_container ]
        [ messages_panel
        ; compose_panel
        ; Vdom.Node.sexp_for_debugging ([%sexp_of: Rpc_effect.Status.t] connection_status)
        ]
    ]
;;
