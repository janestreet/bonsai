open! Core
open Bonsai.For_open

(** [t] is roughly equivalent to [Core_bench_js.Test.t], but can also be used to obtain
    [profile]s of the benchmarks. See [profile] below for more details. *)
type t

module Interaction : sig
  (** A ['action Interaction.t] represents an interaction that occurs with a
      [Component] whose action type is ['action]. *)
  type 'action t

  (** [change_input] sets the given [Var.t] to the supplied value. *)
  val change_input : 'a Bonsai.Var.t -> 'a -> _ t

  (** [inject] calls the [inject_action] function for the component being benchmarked
      with the supplied action. *)
  val inject : 'action -> 'action t

  (** [advance_clock_by] advances the current clock by the supplied amount. Note that the
      clock is not reset between benchmark runs. *)
  val advance_clock_by : Time_ns.Span.t -> _ t

  (** [stabilize] forces a stabilization of the incremental graph for the component being
      benchmarked. *)
  val stabilize : _ t

  (** [reset_model] resets the benchmarked component's model back to the value it was at
      the beginning of the benchmark. *)
  val reset_model : _ t

  (** [profile] indicates that the [profile] function below should print a snapshot of the
      time spent in each part of your computation since the previous snapshot.

      Note: profiling interactions are filtered out in [benchmark] runs, so you needn't
      worry about
  *)
  val profile : name:string -> _ t

  (** [many] is used to create lists of interactions all at once. Interactions created
      this way will get flattened prior to benchmark runtime, so that there isn't
      performance cost to using the constructor. *)
  val many : 'action t list -> 'action t

  (** [many_with_stabilizations] is similar to many, but intersperses [stabilize]s in the
      supplied list of actions. *)
  val many_with_stabilizations : 'action t list -> 'action t
end

(** [create] produces a benchmark which performs [interactions] on [component].
    The computation is shared between runs within the benchmark runner. Since they are
    run a non-deterministic amount of times, benchmarks created this way should either
    have an interaction which is idempotent on the state, or have similar performance
    when the interaction is repeated many times. *)
val create
  :  ?clock:Ui_incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [create_with_resetter] is equivalent to calling [create], with interactions equal to
    [Interaction.many interactions; Interaction.reset_model; Interaction.stabilize]. *)
val create_with_resetter
  :  ?clock:Ui_incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [benchmark] works identically to [Core_bench_js.bench], but ensures that [Observer]s
    involved in benchmarks are cleaned up between consecutive benchmarks. *)
val benchmark
  :  ?run_config:Core_bench_js.Run_config.t
  -> ?analysis_configs:Core_bench_js.Analysis_config.t list
  -> ?display_config:Core_bench_js.Display_config.t
  -> ?save_to_file:(Core_bench_js.Measurement.t -> string)
  -> ?libname:string
  -> t list
  -> unit

(** [profile] runs a given [t] as an instrumented computation, and provides snapshots of
    how much time is spent within different parts of bonsai code. It also provides
    statistics on incremental overhead.

    Note: because [profile] runs on an instrumented computation, the total running time
    of the test may be higher. Furthermore, because [profile] only runs the computation
    once, timing may vary between runs. It is useful for drilling into slow benchmarks,
    but [benchmark] should be the source of truth for timing interactions. *)
val profile : t list -> unit
