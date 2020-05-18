open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input = struct
  type t =
    { room_list : Room.t list
    ; refresh_rooms : unit Effect.t
    ; change_room : Room.t -> unit Effect.t
    }
  [@@deriving fields]

  let create = Fields.create
end

let view { Input.room_list; refresh_rooms; change_room } =
  let room_header =
    Vdom.Node.h2
      []
      [ Vdom.Node.text "Rooms"
      ; Vdom_input_widgets.Button.simple
          ~extra_attrs:[ Vdom.Attr.id "refresh-button" ]
          ~on_click:(fun () -> Effect.inject_ignoring_response refresh_rooms)
          "↻"
      ]
  in
  let room_switching_buttons =
    List.map room_list ~f:(fun room ->
      Vdom_input_widgets.Button.simple
        ~on_click:(fun () -> Effect.inject_ignoring_response (change_room room))
        (Room.to_string room))
  in
  Vdom.Node.div
    [ Vdom.Attr.id "room-list-panel" ]
    ([ room_header ] @ room_switching_buttons)
;;

let component = Bonsai.pure ~f:view
