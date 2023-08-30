open! Core

(** A simple data structure for keeping track of a focused element within a list. The
    focus wraps around so that calling [prev] when the focus is the head of the list will
    cause the focus to move to the end. There is always a focused element -- i.e. there is
    no [blur] operation. *)

type 'a t [@@deriving compare, equal, sexp]

val of_nonempty_list_exn : 'a list -> 'a t
val next : 'a t -> 'a t
val prev : 'a t -> 'a t
val current_focus : 'a t -> 'a

(** O(n). Find the first element matching [f] in the list, and make that the focus. *)
val set : 'a t -> f:('a -> bool) -> 'a t option
