open! Core
open! Import

module Warning : sig
  type t

  val to_string : t -> string
  val relative_to : Source_code_position.t -> t -> t
end

(** Produces a list of missed optimization opportunities. It currently checks for the
    following:
    - Unfolded constants
    - State_machine1 to State_machine0 transformation *)
val list_warnings : _ Computation.t -> Warning.t list
