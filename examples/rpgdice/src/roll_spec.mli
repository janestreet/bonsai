open! Core

module Die : sig
  type t [@@deriving bin_io, compare, equal, sexp_of]

  val of_int : int -> t
end

type t [@@deriving bin_io, compare, equal, sexp]

val roll : ?random_state:Random.State.t -> t -> Roll_result.t
val of_dice_and_const : (int * Die.t) list -> int -> t

(** {1 String representation}

    https://en.wikipedia.org/wiki/Dice_notation *)

val of_string : string -> t
val to_string_hum : t -> string
