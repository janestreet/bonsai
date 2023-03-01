open! Core

type t =
  | Selected
  | Unselected
[@@deriving compare, sexp, equal]

val toggle : t -> t
