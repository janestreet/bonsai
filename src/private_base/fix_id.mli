open! Core

type 'a t [@@deriving sexp_of]

val create : unit -> 'a t
val type_id : 'a t -> 'a Type_equal.Id.t
