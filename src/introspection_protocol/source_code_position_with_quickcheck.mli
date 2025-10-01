open! Core

type t = Source_code_position.t [@@deriving sexp, quickcheck, compare, equal]
