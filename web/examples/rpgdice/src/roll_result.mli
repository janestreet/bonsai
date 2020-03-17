open! Core_kernel
open! Import

module Die : sig
  type t =
    { num_faces : int
    ; result : int
    }
  [@@deriving bin_io, compare, fields, sexp_of]
end

type t =
  { dice : Die.t list
  ; const : int
  }
[@@deriving bin_io, compare, sexp_of]

val to_int : t -> int
val to_string_hum : t -> string
