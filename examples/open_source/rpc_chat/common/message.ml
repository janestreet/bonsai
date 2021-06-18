open! Core

type t =
  { room : Room.t
  ; author : string
  ; contents : string
  }
[@@deriving sexp, bin_io, fields]
