open! Core
open! Import

(** A tracker for actions being applied during the main loop of a Bonsai app, to reduce
    unnecessary stabilizations.

    When a dynamic action (from a [state_machine1]) is applied, the incremental graph
    needs to be stabilized to witness the most up-to-date value of its input. However, if
    we can prove that the input is already up-to-date, we can skip the stabilization (i.e.
    it's an unnecessary stabilization).

    The tracker keeps track of what parts of the Bonsai graph have had actions applied
    since the most recent stabilization and tells Bonsai to skip stabilization if there is
    a dynamic action with an input that the tracker can prove is up-to-date.

    This optimization is particularly helpful when setting forms for highly nested types,
    which generally schedule lots of dynamic actions that are independent of one another. *)

(** [t] contains a phantom type representing the action type of the top-level Bonsai
    computation. It uses this phantom type to ensure the structure of the stabilization
    tracker matches the structure of the actions being applied. *)
type 'action t

(** An empty [t]. Takes a unit parameter because the data structure is mutable. *)
val empty : unit -> _ t

(** Insert an action into the tracker. *)
val insert : 'action t -> 'action Action.t -> unit

(** Return whether or not a stabilization is required before applying the supplied action. *)
val requires_stabilization : 'action t -> 'action Action.t -> bool

(** Mark an unknown part of the incremental graph as requiring stabilization. Used to
    track [Bonsai.Var.t] modification. *)
val mark_incremental_dirty : unit -> unit

(** Tell [t] to update its knowledge of the Incremental stats after a stabilization has
    occurred. *)
val mark_stabilization : _ t -> unit

module For_testing : sig
  module Stats : sig
    type t =
      { mutable num_stabilize : int
      ; mutable num_don't_stabilize : int
      ; mutable num_stabilize_caused_by_vars : int
      ; mutable num_prunes_run : int
      ; mutable num_branches_pruned : int
      }
  end

  val start_debugging : _ t -> unit
  val num_generations_for_pruning : int
  val display_stats : _ t -> unit
end
