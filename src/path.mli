open! Core_kernel
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
    | Enum of Keyed.t
  [@@deriving sexp_of, compare]
end

type t [@@deriving compare, sexp_of]

val empty : t
val append : t -> Elem.t -> t
