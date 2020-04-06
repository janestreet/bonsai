open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input : sig
  type t

  val create
    :  room_list:Room.t list
    -> inject_refresh_rooms_list:(unit -> Vdom.Event.t)
    -> inject_change_room:(Room.t -> Vdom.Event.t)
    -> t
end

val component : (Input.t, Vdom.Node.t) Bonsai.t
