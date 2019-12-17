open! Core_kernel
open! Async_kernel
open Bonsai_chat_common

type t =
  | Refresh_rooms
  | Switch_room of Room.t
  | Send_message of string
