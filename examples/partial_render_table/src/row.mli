open! Core

type t =
  { symbol : string
  ; edge : float
  ; max_edge : float
  ; bsize : int
  ; bid : float
  ; ask : float
  ; asize : int
  ; position : int
  ; last_fill : Time_ns.t option
  ; trader : string
  }
[@@deriving compare, fields, typed_fields]

val many_random : int -> t String.Map.t
