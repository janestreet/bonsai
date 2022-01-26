open! Core

val enable : unit -> unit
val disable : unit -> unit

module Kind : sig
  type t =
    | Apply_action
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
    | Assoc_apply_actions
    | Path
    | Impossible_apply_action
    | Lifecycle_apply_action_pair
end

val annotate : Kind.t -> 'a Ui_incr.t -> unit
val annotate_packed : Kind.t -> Ui_incr.Packed.t -> unit
val attribute : Source_code_position.t option -> 'a Ui_incr.t -> unit
val attribute_packed : Source_code_position.t option -> Ui_incr.Packed.t -> unit
