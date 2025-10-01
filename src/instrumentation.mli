open! Core
open! Import

(** This module allows configuring instrumentation of the Bonsai computation graph.

    If [instrument_for_profiling], each node in the [Computation.t] will be wrapped with
    calls to [start_timer] and [stop_timer]. The string passed to both these is intended
    to be the names for performance entries sent to the browser. The strings contain the
    [Node_path] to which the time corresponds. When these strings eventually get retrieved
    via a PerformanceObserver, the id in the string can be retrieved via
    [extract_node_path_from_entry_label]. (We do things this way because we want also want
    to include the node_type in this string as well, so that looking at measurements in
    Chrome's profiling tools is easier).

    Additionally, if [instrument_for_profiling] is enabled, [set_latest_graph_info] will
    be called whenever the graph info gets updated. In practice, this should occur when
    the computation watcher is enabled / disabled, or when a [Lazy] gets forced.

    If [instrument_for_computation_watcher], any enabled [Debug.watch_computation] nodes
    will be able to run. *)

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
    ; set_latest_graph_info : Graph_info.Stable.V3.t -> unit
    ; computation_watcher_queue : Computation_watcher.Node.t Queue.t
    ; start_timer : 'timeable_event -> 'timer
    ; stop_timer : 'timer -> unit
    }
end

val create_computation_with_instrumentation
  :  (string, _) Config.t
  -> f:
       (('model, 'action, 'action_input, 'result, unit) Computation.eval_fun
        -> 'a Ui_incr.Incr.t)
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> computation:'result Computation.t
  -> ('model, 'action, 'action_input, 'result, unit) Computation.info
  -> 'a Ui_incr.Incr.t

val extract_node_path_from_entry_label : string -> Node_path.t option

module For_testing : sig
  val instrument_for_profiling
    :  set_latest_graph_info:(Graph_info.t -> unit)
    -> start_timer:(string -> 'a)
    -> stop_timer:('a -> unit)
    -> 'b Computation.t
    -> 'b Computation.t
end
