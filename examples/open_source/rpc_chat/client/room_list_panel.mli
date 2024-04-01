open! Core
open! Bonsai_web.Cont
open Bonsai_chat_open_source_common

val component
  :  room_list:Room.t list Bonsai.t
  -> refresh_rooms:unit Effect.t
  -> change_room:(Room.t -> unit Effect.t)
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
