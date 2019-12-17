open! Core_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input : sig
  type t =
    { rooms : Room.t list
    ; current_room : Room.t option
    ; messages : Message.t list
    }

  val default : t
end

module Model : sig
  type t = Compose.Model.t
end

val component
  : ( (Input.t, Outgoing.t) Start.App_input.t
    , Model.t
    , Nothing.t Start.App_result.t )
      Bonsai.t
