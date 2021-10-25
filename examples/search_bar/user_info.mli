open! Core

type t =
  { name : string
  ; int_id : int
  }
[@@deriving compare, equal, fields, sexp]

val sample_data : t String.Map.t Lazy.t
