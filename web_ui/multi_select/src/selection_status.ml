open! Core

type t =
  | Selected
  | Unselected
[@@deriving compare, equal, sexp]

let toggle = function
  | Selected -> Unselected
  | Unselected -> Selected
;;
