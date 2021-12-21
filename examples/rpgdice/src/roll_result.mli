open! Core

module Die : sig
  type t =
    { num_faces : int
    ; result : int
    }
  [@@deriving bin_io, compare, equal, fields, sexp]
end

type t =
  { dice : Die.t list
  ; const : int
  }
[@@deriving bin_io, compare, equal, sexp]

val to_int : t -> int
val to_string_hum : t -> string
