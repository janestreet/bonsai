open! Core

(** The [Bonsai_bench_scenario] library contains code that is shared between benchmarking
    and testing. *)

module Input : sig
  (** An input to the Bonsai computation you are testing / benchmarking. It can be changed
      during testing using [Interaction.change_input].

      It's secretly a [Bonsai.Expert.Var.t], but we don't expose that because you MUST NOT
      use [Var.t] functions to compute new values. This is because all values that the
      input shall take must be computed before the benchmarks / tests start; otherwise,
      their computation will throw off benchmarks. *)
  type 'a t

  val create : 'a -> 'a t
  val value : 'a t -> 'a Bonsai.t
end

module Interaction : sig
  (** A ['action Interaction.t] represents an interaction that occurs with a Bonsai
      computation whose action type is ['action].

      It's useful for tests and benchmarks. *)
  type 'action t

  (** [update_input] updates the given [Input.t] using the supplied function. *)
  val update_input : 'a Input.t -> f:('a -> 'a) -> _ t

  (** [change_input] sets the given [Input.t] to the supplied value. *)
  val change_input : 'a Input.t -> 'a -> _ t

  (** [inject] calls the [inject_action] function for the component being benchmarked with
      the supplied action. *)
  val inject : 'action -> 'action t

  (** [advance_clock_by] advances the current clock by the supplied amount. Note that the
      clock is not reset between benchmark runs. *)
  val advance_clock_by : Time_ns.Span.t -> _ t

  (** [recompute] forces a stabilization of the incremental graph for the component being
      benchmarked, and triggers lifecycle events. *)
  val recompute : _ t

  (** [reset_model] resets the benchmarked component's model back to the value it was at
      the beginning of the benchmark. *)
  val reset_model : _ t

  (** [profile] indicates that the [profile] function below should print a snapshot of the
      time spent in each part of your computation since the previous snapshot.

      By default, [Bonsai_bench.profile] will take a snapshot after startup, and another
      after running all interactions. [Interaction.profile] can be used to add an
      additional snapshot, in between your other interactions.

      Note: profiling interactions are filtered out when not running
      [Bonsai_bench.profile], so including them is free. *)
  val profile : name:string -> _ t

  (** [many] is used to create lists of interactions all at once. Interactions created
      this way will get flattened prior to benchmark runtime, so that there isn't
      performance cost to using the constructor. *)
  val many : 'action t list -> 'action t

  (** [many_with_recomputes] is similar to many, but intersperses [recompute]s in the
      supplied list of actions. *)
  val many_with_recomputes : 'action t list -> 'action t

  module Finalized : sig
    type 'action t

    val handle
      :  driver:'a Bonsai_driver.t
      -> time_source:Bonsai.Time_source.t
      -> inject_action:('b -> unit Bonsai.Effect.t)
      -> handle_profile:(string -> unit)
      -> 'b t
      -> unit
  end

  val finalize : filter_profiles:bool -> 'a t -> 'a Finalized.t list
end

module Scenario : sig
  (** A [Scenario.t] is a useful pattern for specifying the inputs and interactions for a
      benchmark or interaction [Computation_report] test. It allows instantiating a
      [Var.t] for the test locally, so it gets GCed between runs. *)
  type ('input, 'action) t =
    { initial : 'input
    ; test_name : string
    ; interaction : 'input Input.t -> 'action Interaction.t
    }
end

type ('input, 'output) compare_computation =
  'input Bonsai.t -> Bonsai.graph @ local -> 'output Bonsai.t
