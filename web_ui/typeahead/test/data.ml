open! Core

module T = struct
  type t =
    | Option_A
    | Option_B
    | Option_C
  [@@deriving variants, enumerate, sexp, equal, compare]
end

include T

let to_string = function
  | Option_A -> "Option A"
  | Option_B -> "Option B"
  | Option_C -> "Option C"
;;

include Comparable.Make (T)
