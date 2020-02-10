open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input = struct
  type t =
    { room_list : Room.t list
    ; inject_refresh_rooms_list : unit -> Vdom.Event.t
    ; inject_change_room : Room.t -> Vdom.Event.t
    }
  [@@deriving fields]

  let create = Fields.create
end

let view { Input.room_list; inject_refresh_rooms_list; inject_change_room } =
  let room_header =
    Vdom.Node.h2
      []
      [ Vdom.Node.text "Rooms"
      ; Vdom_input_widgets.Button.simple
          ~extra_attrs:[ Vdom.Attr.id "refresh-button" ]
          ~on_click:(fun () -> inject_refresh_rooms_list ())
          "↻"
      ]
  in
  let room_switching_buttons =
    List.map room_list ~f:(fun room ->
      Vdom_input_widgets.Button.simple
        ~on_click:(fun () -> inject_change_room room)
        (Room.to_string room))
  in
  Vdom.Node.div
    [ Vdom.Attr.id "room-list-panel" ]
    ([ room_header ] @ room_switching_buttons)
;;

let component = Bonsai.pure ~f:view
