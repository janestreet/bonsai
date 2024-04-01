open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai_chat_common

val component
  :  messages:Message.t list Bonsai.t
  -> current_room:Room.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
