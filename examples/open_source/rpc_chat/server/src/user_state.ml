open! Core_kernel
open! Async

type t =
  { user : string
  ; connection : Rpc.Connection.t
  }
