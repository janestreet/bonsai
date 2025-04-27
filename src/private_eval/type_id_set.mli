open! Core
open! Import

(** A type for containing a set of [Type_equal.Id.t]. Typically used to track free
    variables inside a computation *)
type t

(** a polymorphic function for mapping over the set *)
type 'b mapper = { f : 'a. 'a Type_equal.Id.t -> 'b }

(** a polymorphic function for folding over the set *)
type 'acc folder = { f : 'a. 'acc -> 'a Type_equal.Id.t -> 'acc }

(** the empty set *)
val empty : t

(** a singleton set *)
val singleton : _ Type_equal.Id.t -> t

(** returns [true] if the set contains no elements *)
val is_empty : t -> bool

val length : t -> int

(** Adds a type-id to the set. Nothing happens if the set already contains the item. *)
val add : t -> _ Type_equal.Id.t -> t

(** Removes a type-id from the set. Nothing happens if the set doesn't already contain it *)
val remove : t -> _ Type_equal.Id.t -> t

(** computes the union of two sets *)
val union : t -> t -> t

(** Folds over the elements in the set *)
val fold : t -> init:'acc -> 'acc folder -> 'acc

(** maps over the set and sticks the return values in a list *)
val map_to_list : t -> 'a mapper -> 'a list

(** iterates over the elements in the set *)
val iter : t -> unit mapper -> unit
