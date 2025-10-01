open! Core

(** Types to perform gather-time analysis on data-dependencies of computations.
    Data-dependencies can be resolved or unresolved:

    - Unresolved dependencies track a (possibly empty) set of recursive dependencies they
      depend on, introduced when computations call the [~recurse] argument of a
      [Bonsai.fix].
    - Resolved dependencies don't have any recursive dependencies.

    The logic for resolving dependencies (i.e. going from unresolved -> resolved) lives in
    [environment.ml], since we need to track information about the recursive dependencies,
    which gets stored in [Environment.Recursive.t]. *)

(** A set of [Fix_id.t] representing the recursive dependencies of a computation. *)
module Dependencies : sig
  module Item : sig
    type t = private T : 'a Fix_id.t -> t
  end

  type t

  val is_empty : t -> bool
  val fold : t -> init:'a -> f:('a -> Item.t -> 'a) -> 'a
end

type resolved = private Resolved [@@deriving sexp_of]
type unresolved = private Unresolved [@@deriving sexp_of]

module Single : sig
  type 'resolution t =
    | Yes_or_maybe : 'a t
    | No : 'a t
    | Depends_on_recursion : unresolved t
  [@@deriving sexp_of]
end

(** [t] tracks non-recursive information about what a computation contains. This
    information can be built from the bottom up at [gather] time. *)
type 'resolution t = private
  { path : 'resolution Single.t
  ; lifecycle : 'resolution Single.t
  ; input : 'resolution Single.t
  }
[@@deriving sexp_of]

module Resolved : sig
  type nonrec t = resolved t

  val create
    :  path:resolved Single.t
    -> lifecycle:resolved Single.t
    -> input:resolved Single.t
    -> t

  val merge : t -> t -> t
  val path : t -> resolved Single.t
  val lifecycle : t -> resolved Single.t
  val input : t -> resolved Single.t
  val both_use_path : t -> t -> bool
end

module Unresolved : sig
  type nonrec t = private
    { contains : unresolved t
    ; recursive_dependencies : Dependencies.t
    }

  val merge : t -> t -> t

  val non_recursive
    :  path:resolved Single.t
    -> lifecycle:resolved Single.t
    -> input:resolved Single.t
    -> t

  val lazy_ : t
  val recursive : 'a Fix_id.t -> t
  val remove_dependency : t -> on:'a Fix_id.t -> t
  val of_resolved : Resolved.t -> t
  val resolved_equivalent : t -> Resolved.t
end
