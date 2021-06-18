open! Core
open! Bonsai_web
open Bonsai_chat_open_source_common

val component
  :  room_list:Room.t list Value.t
  -> refresh_rooms:unit Effect.t
  -> change_room:(Room.t -> unit Effect.t)
  -> Vdom.Node.t Computation.t
