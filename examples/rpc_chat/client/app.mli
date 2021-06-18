open! Core
open! Bonsai_web
open Bonsai_chat_common

val component
  :  room_list:Room.t list Value.t
  -> current_room:Room.t option Value.t
  -> messages:Message.t list Value.t
  -> refresh_rooms:unit Effect.t
  -> change_room:(Room.t -> unit Effect.t)
  -> send_message:(room:Room.t -> contents:string -> unit Effect.t)
  -> Vdom.Node.t Computation.t
