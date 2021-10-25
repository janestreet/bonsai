open! Core

type t =
  | Option_A
  | Option_B
  | Option_C
[@@deriving variants, enumerate, sexp, equal, compare]

val to_string : t -> string

include Comparable.S with type t := t
