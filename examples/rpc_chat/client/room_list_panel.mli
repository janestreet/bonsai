open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

val component
  :  room_list:Room.t list Bonsai.Value.t
  -> refresh_rooms:unit Effect.t
  -> change_room:(Room.t -> unit Effect.t)
  -> Vdom.Node.t Bonsai.Computation.t
