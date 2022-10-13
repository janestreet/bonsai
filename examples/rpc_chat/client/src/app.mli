open! Core
open! Bonsai_web
open Bonsai_chat_common

val component
  :  current_room:Room.t option Value.t
  -> messages:Message.t list Value.t
  -> change_room:(Room.t -> unit Effect.t)
  -> obfuscate_message:(string -> string)
  -> Vdom.Node.t Computation.t
