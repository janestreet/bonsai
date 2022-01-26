open! Core
open! Import

(** Represents the shortest unambiguous path through the Computation.t data
    structure, including the path through any contained [Value.t]s. Several of
    the constructors in Computation.t only contain a single inner computation
    or value; thus, keeping track of these segments in all the paths is often
    unnecessary. Thus, we can more properly model this path as with two parts:

    - the list of choices made; nodes which do not require a choice do not make
      it into the list.
    - a number representing the amount of nodes descended into after the last
      choice point.

    Whenever a choice point gets added, we reset the number of nodes descended
    back to 0, since that number is no longer helpful for keeping the paths
    unique. *)
type t [@@deriving bin_io, sexp]

include Comparable.S_binable with type t := t

(** The [of_string] and [to_string] functions operate on short, yet
    semi-human-readable strings.

    For example:
    {[
      { choices = [ 1; 2; 3]; descend = 4 }
    ]}
    is serialized to "1-2-3_4" *)
include Stringable.S with type t := t

type builder

(** The empty node path. *)
val empty : builder

(** Adds a choice point to that input path. The provided number says which of
    the choices at that point was taken. When traversing a computation or
    value, if any case has multiple recursive calls, you should add a choice
    point to the path built up in each of those calls, with each call using a
    different number. *)
val choice_point : builder -> int -> builder

(** Adds an extra segment to the input path. All such segments get forgotten
    when the next choice point is added. *)
val descend : builder -> builder

val finalize : builder -> t
