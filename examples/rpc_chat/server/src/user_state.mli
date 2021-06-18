open! Core
open! Async

type t =
  { user : Krb.Principal.Name.t
  ; connection : Rpc.Connection.t
  }
