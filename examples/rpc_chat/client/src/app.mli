open! Core
open! Bonsai_web.Cont
open Bonsai_chat_common

val component
  :  current_room:Room.t option Bonsai.t
  -> messages:Message.t list Bonsai.t
  -> change_room:(Room.t -> unit Effect.t)
  -> obfuscate_message:(string -> string)
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
