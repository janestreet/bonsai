open! Core
open! Import

(** Wraps each node in a [Computation.t] with calls to [start_timer] and
    [stop_timer]. The string passed to both these is intended to be the names
    for performance entries sent to the browser. The strings contain the
    [Node_path] to which the time corresponds. When these strings eventually get
    retrieved via a PerformanceObserver, the id in the string can be retrieved
    via [Node_path.extract_from_entry_label]. (We do things this way because we
    want also want to include the node_type in this string as well, so that
    looking at measurements in Chrome's profiling tools is easier).

    [on_graph_update] is called whenever the graph info gets updated. For
    example (in fact, the only example), when a [Lazy] gets forced, we must
    expand the graph to include what was inside the Lazy. *)
val instrument_computation
  :  ('model, 'dynamic_action, 'static_action, 'result) Computation.t
  -> start_timer:(string -> unit)
  -> stop_timer:(string -> unit)
  -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t

(** Same as [instrument_computation] but operating on packed computations instead. *)
val instrument_packed
  :  'a Computation.packed
  -> start_timer:(string -> unit)
  -> stop_timer:(string -> unit)
  -> 'a Computation.packed

val extract_node_path_from_entry_label : string -> Node_path.t option
