open! Core
open Composition_infix

module Die = struct
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

let to_int { dice; const } = List.sum (module Int) dice ~f:Die.result + const

let to_string_hum { dice; const } =
  let dice = String.concat ~sep:" + " (List.map dice ~f:(Die.result >> Int.to_string)) in
  match const with
  | 0 -> dice
  | n -> Int.to_string n ^ " + " ^ dice
;;
