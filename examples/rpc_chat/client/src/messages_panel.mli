open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

val component
  :  messages:Message.t list Value.t
  -> current_room:Room.t Value.t
  -> Vdom.Node.t Computation.t
