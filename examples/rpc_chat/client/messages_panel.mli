open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Bonsai_chat_common

val component
  :  messages:Message.t list Bonsai.Proc.Value.t
  -> current_room:Room.t Bonsai.Proc.Value.t
  -> Vdom.Node.t Bonsai.Proc.Computation.t
