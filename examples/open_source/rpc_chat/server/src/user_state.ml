open! Core
open! Async

type t =
  { user : string
  ; connection : Rpc.Connection.t
  }
