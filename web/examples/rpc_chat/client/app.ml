open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input = struct
  type t =
    { rooms : Room.t list
    ; current_room : Room.t option
    ; messages : Message.t list
    }

  let default = { rooms = []; current_room = None; messages = [] }
end

module Model = Compose.Model

let convert_room_list_input app_input =
  let input = Start.App_input.input app_input in
  let inject = Start.App_input.inject_outgoing app_input in
  Room_list_panel.Input.create
    ~room_list:input.Input.rooms
    ~inject_refresh_rooms_list:(fun () -> inject Outgoing.Refresh_rooms)
    ~inject_change_room:(fun s -> inject (Outgoing.Switch_room s))
;;

let convert_messages_panel_input app_input =
  let input = Start.App_input.input app_input in
  let { Input.current_room; messages; _ } = input in
  let current_room =
    Option.value current_room ~default:(Room.of_string "No room selected")
  in
  Messages_panel.Input.create ~current_room ~messages
;;

let convert_compose_panel_input app_input =
  let inject = Start.App_input.inject_outgoing app_input in
  let inject_send_message message = message |> Outgoing.Send_message |> inject in
  Compose.Input.create ~inject_send_message
;;

let component =
  let open Bonsai.Infix in
  let%map.Bonsai rooms_list =
    convert_room_list_input @>> Room_list_panel.component |> Bonsai.Project.Model.ignore
  and compose_panel = convert_compose_panel_input @>> Compose.component
  and messages_panel =
    convert_messages_panel_input @>> Messages_panel.component
    |> Bonsai.Project.Model.ignore
  in
  let view =
    Vdom.Node.div
      [ Vdom.Attr.id "container" ]
      [ rooms_list
      ; Vdom.Node.div
          [ Vdom.Attr.id "message-container" ]
          [ messages_panel; compose_panel ]
      ]
  in
  let inject_incoming = Nothing.unreachable_code in
  Start.App_result.create ~view ~inject_incoming
;;
