open! Core

val enable : unit -> unit
val disable : unit -> unit

module Kind : sig
  type t =
    | Input
    | Value
    | Result
    | Lifecycle
    | Empty_lifecycle
    | Model
    | Model_and_input
    | Switch_model
    | Assoc_key
    | Assoc_input
    | Assoc_results
    | Assoc_lifecycles
    | Assoc_inputs
    | Path
    | Lifecycle_apply_action_pair

  val name : t -> string
end

(** [on_incr_annotation] registers a callback that will run whenever Bonsai annotates an
    incremental node. Annotation happens:

    - On node creation
    - When a node is registered as an input or result to a [Snapshot.t]

    A single node may be annotated multiple times, e.g. if it is both a model and a
    result. *)
val on_incr_annotation
  :  (here:Source_code_position.t -> Kind.t -> Ui_incr.Packed.t -> unit)
  -> unit

(** [annotate] will run all [on_incr_annotation] listeners. *)
val annotate : here:Source_code_position.t -> Kind.t -> 'a Ui_incr.t -> unit

val annotate_packed : here:Source_code_position.t -> Kind.t -> Ui_incr.Packed.t -> unit

module Counts : sig
  (** Values of this type contain a counter for every kind of incremental node created by
      Bonsai. The numbers are tracked since the start of the program, so we recommend that
      you call [diff] to see how the counts have changed between two calls to [current].

      IMPORTANT: this number is not decremented when nodes are deleted, because we don't
      have a performant way of doing that.

      Some of the counts may be double-counted! For example, a state's model may be
      considered a "model" incremental as well as a "result" incremental if it's returned
      from a component. *)
  type t [@@deriving sexp_of]

  val current : unit -> t
  val empty : unit -> t
  val diff : before:t -> after:t -> t
  val incr : t -> Kind.t -> unit
end

val attribute : Source_code_position.t -> 'a Ui_incr.t -> unit
val attribute_packed : Source_code_position.t -> Ui_incr.Packed.t -> unit

module For_profiling : sig
  module Counts : sig
    type t =
      { input : int
      ; value : int
      ; result : int
      ; lifecycle : int
      ; empty_lifecycle : int
      ; model : int
      ; model_and_input : int
      ; switch_model : int
      ; assoc_key : int
      ; assoc_input : int
      ; assoc_results : int
      ; assoc_lifecycles : int
      ; assoc_inputs : int
      ; path : int
      ; lifecycle_apply_action_pair : int
      }
    [@@deriving sexp, equal]

    val t_of_opaque_counts : Counts.t -> t
    val diff : before:t -> after:t -> t
  end
end
