open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_chat_common

module Style =
  [%css.raw
    {|
  .room_list_panel {
       width: 150px;
       display:flex;
       flex-direction:column;
   }

  .refresh_button {
      background:white;
      border:0;
  }
|}]

let component ~room_list ~refresh_rooms ~change_room =
  let%arr room_list = room_list in
  let room_header =
    Vdom.Node.h2
      [ Vdom.Node.text "Rooms"
      ; Vdom_input_widgets.Button.simple
          ~extra_attrs:[ Vdom.Attr.class_ Style.refresh_button ]
          ~on_click:(fun () -> refresh_rooms)
          "↻"
      ]
  in
  let room_switching_buttons =
    List.map room_list ~f:(fun room ->
      let on_click () = change_room room in
      Vdom_input_widgets.Button.simple ~on_click (Room.to_string room))
  in
  Vdom.Node.div
    ~attr:(Vdom.Attr.class_ Style.room_list_panel)
    ([ room_header ] @ room_switching_buttons)
;;
