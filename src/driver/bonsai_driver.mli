open! Core
module Incr = Ui_incr
module Instrumentation = Instrumentation

type 'r t

(** Builds a new driver for a bonsai component. *)
val create
  :  here:[%call_pos]
  -> ?optimize:bool
  -> instrumentation:
       (Instrumentation.Timeable_event.t, _) Bonsai.Private.Instrumentation.Config.t
  -> time_source:Bonsai.Time_source.t
  -> (local_ Bonsai.graph -> 'r Bonsai.t)
  -> 'r t

(** The "main loop" of a bonsai handle is
    1. flush - dequeue events and process actions
    2. result - compute the final value of the computation
    3. trigger_lifecycles - run any lifecycle events for this stabilization *)

(** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
val flush
  :  ?log_before_action_application:(action_sexp:Sexp.t Lazy.t -> unit)
  -> ?log_on_skipped_stabilization:(action_sexp:Sexp.t Lazy.t -> unit)
  -> 'a t
  -> unit

(** retrieves the computed result of the bonsai application *)
val result : 'r t -> 'r

(** Triggers all lifecycle events that need to be run. Lifecycle events are run in this
    order:
    1. component deactivations
    2. component activations
    3. after display, which includes things like on_change *)
val trigger_lifecycles : _ t -> unit

(** Schedules an event *)
val schedule_event : _ t -> unit Ui_effect.t -> unit

(** returns true if the lifecycle collection has anything that it wants to do before the
    display is processed. *)
val has_before_display_events : _ t -> bool

(** returns true if the lifecycle collection has anything that it wants to do after the
    display is processed. *)
val has_after_display_events : _ t -> bool

module Expert : sig
  (** An incremental handle on the result of the computation *)
  val result_incr : 'r t -> 'r Incr.t

  (** An incremental handle on the lifecycle colletion for the computation *)
  val lifecycle_incr : _ t -> Incr.Packed.t

  (** An incremental handle on the action handler for the computation *)
  val action_input_incr : _ t -> Incr.Packed.t

  (** Access the time_source that was used when creating the handle *)
  val time_source : _ t -> Bonsai.Time_source.t

  (** Kill everything in this handle by invalidating all the incremental observers. After
      this function is called, other functions in this module may begin to throw
      exceptions without warning. *)
  val invalidate_observers : _ t -> unit

  (** Serializes the model of the component to a sexp. Used only for debugging *)
  val sexp_of_model : _ t -> Sexp.t

  (** Resets the model of the component back to its initial model. Used only for
      benchmarking *)
  val reset_model_to_default : _ t -> unit

  (** Starts printing the paths of actions that are applied during tests *)
  val print_actions : _ t -> unit

  (** Start printing when stabilizations occur during action application in tests *)
  val print_stabilizations : _ t -> unit

  (** Print the stats associated with the stabilization tracker *)
  val print_stabilization_tracker_stats : _ t -> unit
end

module For_testing : sig
  val dump_dot : _ t -> string

  (** returns the computation that is actually being run (it has optimizations applied if
      applicable) *)
  val running_computation : 'r t -> 'r Bonsai.Private.Computation.t
end
