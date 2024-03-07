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
    | Assoc_key
    | Assoc_input
    | Assoc_results
    | Assoc_lifecycles
    | Assoc_inputs
    | Path
    | Lifecycle_apply_action_pair
end

module Counts : sig
  (** Values of this type contain a counter for every kind of incremental 
      node that Bonsai is aware of. The numbers are tracked since the 
      start of the program, so we recommend that you call [diff] to see how 
      the counts have changed between two calls to [current].

      Some of the counts may be double-counted!  For example, a state's 
      model may be considered a "model" incremental as well as a "result" 
      incremental if it's returned from a component. *)
  type t [@@deriving sexp_of]

  val current : unit -> t
  val diff : before:t -> after:t -> t
end

val annotate : Kind.t -> 'a Ui_incr.t -> unit
val annotate_packed : Kind.t -> Ui_incr.Packed.t -> unit
val attribute : Source_code_position.t option -> 'a Ui_incr.t -> unit
val attribute_packed : Source_code_position.t option -> Ui_incr.Packed.t -> unit
