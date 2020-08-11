open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

val component
  :  messages:Message.t list Bonsai.Value.t
  -> current_room:Room.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
