open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input = struct
  type t =
    { rooms : Room.t list
    ; current_room : Room.t option
    ; messages : Message.t list
    ; refresh_rooms : unit Effect.t
    ; switch_room : Room.t -> unit Effect.t
    ; send_message : room:Room.t -> contents:string -> unit Effect.t
    }

  let default =
    { rooms = []
    ; current_room = None
    ; messages = []
    ; refresh_rooms = Effect.never
    ; switch_room = (fun _ -> Effect.never)
    ; send_message = (fun ~room:_ ~contents:_ -> Effect.never)
    }
  ;;
end

module Model = Compose_message.Model

let convert_room_list_input (input : Input.t) =
  Room_list_panel.Input.create
    ~room_list:input.rooms
    ~refresh_rooms:input.refresh_rooms
    ~change_room:input.switch_room
;;

let convert_messages_panel_input (input : Input.t) =
  let { Input.current_room; messages; _ } = input in
  let current_room =
    Option.value current_room ~default:(Room.of_string "No room selected")
  in
  Messages_panel.Input.create ~current_room ~messages
;;

let convert_compose_panel_input (input : Input.t) =
  let send_message =
    match input.current_room with
    | Some room -> fun contents -> input.send_message ~room ~contents
    | None -> Fn.const Effect.never
  in
  Compose_message.Input.create ~send_message
;;

let component =
  let open Bonsai.Infix in
  let%map.Bonsai rooms_list = convert_room_list_input @>> Room_list_panel.component
  and compose_panel = convert_compose_panel_input @>> Compose_message.component
  and messages_panel = convert_messages_panel_input @>> Messages_panel.component in
  Vdom.Node.div
    [ Vdom.Attr.id "container" ]
    [ rooms_list
    ; Vdom.Node.div [ Vdom.Attr.id "message-container" ] [ messages_panel; compose_panel ]
    ]
;;
