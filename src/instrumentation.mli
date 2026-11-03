open! Core
open! Import

(** This module allows configuring instrumentation of the Bonsai computation graph.

    If [instrument_for_profiling], each node in the [Computation.t] will be wrapped with
    calls to [start_timer] and [stop_timer].

    If [instrument_for_computation_watcher], any enabled [Debug.watch_computation] nodes
    will be able to run. *)

module Profiled_event : sig
  module Node_id : Identifiable.S

  module Metadata : sig
    type t =
      | Map of { arity : int }
      | Assoc_by
      | Incr_compute
      | Apply_action of
          { action_sexp : Sexp.t option
          ; kind : [ `Static | `Dynamic | `Wrap ]
          }
    [@@deriving sexp, equal]

    val node_type : t -> string
  end

  type t =
    { node_id : Node_id.t
    ; metadata : Metadata.t
    ; here : Source_code_position.t
    }
  [@@deriving sexp, equal]

  val to_entry_label : t -> string
  val of_entry_label : string -> t option

  module For_testing : sig
    val to_string : t -> string
  end
end

module Profiling : sig
  type t =
    | Profiling
    | Not_profiling
  [@@deriving sexp]
end

module Watching : sig
  type t =
    | Watching
    | Not_watching
  [@@deriving sexp_of]
end

module Config : sig
  type ('timeable_event, 'timer) t =
    { instrument_for_computation_watcher : Watching.t Incr.t
    ; instrument_for_profiling : Profiling.t Incr.t
    ; computation_watcher_queue : Computation_watcher.Node.t Queue.t
    ; start_timer : 'timeable_event -> 'timer
    ; stop_timer : 'timer -> unit
    }
end

val create_computation_with_instrumentation
  :  (Profiled_event.t, _) Config.t
  -> f:
       (('model, 'action, 'action_input, 'result, unit) Computation.info
        -> 'a Ui_incr.Incr.t)
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> computation:'result Computation.t
  -> ('model, 'action, 'action_input, 'result, unit) Computation.info
  -> 'a Ui_incr.Incr.t

module For_testing : sig
  val instrument_for_profiling
    :  start_timer:(Profiled_event.t -> 'a)
    -> stop_timer:('a -> unit)
    -> 'b Computation.t
    -> 'b Computation.t
end
