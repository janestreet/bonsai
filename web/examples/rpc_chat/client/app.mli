open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input : sig
  type t =
    { rooms : Room.t list
    ; current_room : Room.t option
    ; messages : Message.t list
    ; refresh_rooms : unit Effect.t
    ; switch_room : Room.t -> unit Effect.t
    ; send_message : room:Room.t -> contents:string -> unit Effect.t
    }

  val default : t
end

module Model : sig
  type t = Compose_message.Model.t
end

val component : (Input.t, Vdom.Node.t) Bonsai.t
