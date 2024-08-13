open! Core

type t =
  | T :
      { key : 'k
      ; id : 'k Type_equal.Id.t
      ; compare : 'k -> 'k -> int
      }
      -> t
[@@deriving sexp_of]

val create : key:'k -> id:'k Type_equal.Id.t -> compare:('k -> 'k -> int) -> t

include Comparable.S_plain with type t := t
