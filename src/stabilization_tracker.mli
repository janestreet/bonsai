open! Core

type 'action t

(** An empty [t]. Takes a unit parameter because the data structure is mutable. *)
val empty : unit -> _ t

(** Insert an action into the tracker. *)
val insert : 'action t -> 'action Action.t -> unit

(** Return whether or not a stabilization is required before applying the supplied
    action. *)
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
