open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input : sig
  type t

  val create
    :  room_list:Room.t list
    -> refresh_rooms:unit Effect.t
    -> change_room:(Room.t -> unit Effect.t)
    -> t
end

val component : (Input.t, Vdom.Node.t) Bonsai.t
