open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

module Input : sig
  type t

  val create : current_room:Room.t -> messages:Message.t list -> t
end

val component : (Input.t, Vdom.Node.t) Bonsai.t
