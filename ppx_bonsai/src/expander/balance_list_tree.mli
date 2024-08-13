open! Core

type 'a t = private
  | Leaf of 'a
  | Node of 'a t Nonempty_list.t
[@@deriving sexp_of]

(** Balances the given list. Each sublist's length within the tree has the property of
    being <= n.

    The function returns an error if the length of the list is < 1 or if n <= 0. *)
val balance : n:int -> 'a list -> 'a t Nonempty_list.t Or_error.t
