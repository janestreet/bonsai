open! Core
open! Import

module Elem : sig
  module Keyed : T

  val keyed : compare:('a -> 'a -> int) -> 'a Type_equal.Id.t -> ('a -> Keyed.t) Staged.t

  (** We only have path nodes for places in the computation graph that could
      introduce a branch. *)
  type t =
    | Subst_from
    | Subst_into
    | Assoc of Keyed.t
    | Switch of int
  [@@deriving sexp_of, compare]
end

type t [@@deriving compare, sexp_of]

include Comparable.S_plain with type t := t

val empty : t
val append : t -> Elem.t -> t

(** Converts the path to a "unique" string that contains only
    lowercase letters and underscores.  This makes it viable for e.g. HTML ids.

    The uniqueness of this string depends on the uniqueness of the sexp
    function for any modules that are being used in "assoc".  The
    invariant that must be upheld by those modules is the following:

    [a != b] implies [sexp_of a != sexp_of b] *)
val to_unique_identifier_string : t -> string
