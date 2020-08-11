open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

let component ~room_list ~refresh_rooms ~change_room =
  let open Bonsai.Let_syntax in
  Bonsai.read
  @@ let%map room_list = room_list in
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
