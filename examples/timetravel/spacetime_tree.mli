open! Core

type 'a t [@@deriving equal, sexp]

module Cursor : sig
  type t [@@deriving equal, sexp]
end

val create : 'a -> 'a t * Cursor.t
val append : 'a t -> Cursor.t -> 'a -> 'a t * Cursor.t
val find : 'a t -> Cursor.t -> 'a
val width : 'a t -> int
val height : 'a t -> int
val traverse : 'a t -> f:(data:'a -> cursor:Cursor.t -> children:'r list -> 'r) -> 'r
