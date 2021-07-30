open! Core
open! Import

(** Wraps operations in [Computation.t] with calls to [start_timer] and [stop_timer].
    The string passed in should be used as a label to identify the function calling [start_timer].
    The calls to [start_timer] and [stop_timer] wrapping a specific operations will be called
    with the same [string]. These strings are not unique between different calls to [start/stop_timer].

    Recursively works with nested [Computation.t]'s.
*)
val instrument_computation
  :  'result Computation.packed
  -> start_timer:(string -> unit)
  -> stop_timer:(string -> unit)
  -> 'result Computation.packed
