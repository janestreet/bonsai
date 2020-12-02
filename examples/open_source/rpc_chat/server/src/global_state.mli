open! Core
open! Async
open Bonsai_chat_open_source_common

type t =
  { message_bus : (Message.t -> unit) Bus.Read_write.t
  ; messages : Message.t Queue.t Room.Table.t
  }

val create : unit -> t
