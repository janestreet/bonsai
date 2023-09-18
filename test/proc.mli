open! Core
open! Import
open Bonsai.For_open

module Result_spec : sig
  module type S = sig
    type t
    type incoming

    val view : t -> string
    val incoming : t -> incoming -> unit Effect.t
  end

  type ('result, 'incoming) t =
    (module S with type t = 'result and type incoming = 'incoming)

  (** [include No_incoming] is a quick way to define a [Result_spec] with no incoming
      events:

      {[
        module Plain_int_result : Result_spec = struct
          type t = int

          let view = Int.to_string

          include Bonsai_test.Proc.Result_spec.No_incoming
        end
      ]}
  *)
  module No_incoming : sig
    type incoming = Nothing.t

    val incoming : _ -> Nothing.t -> unit Effect.t
  end

  module type Sexpable = sig
    type t [@@deriving sexp_of]
  end

  module type Stringable = sig
    type t

    val to_string : t -> string
  end

  val sexp : (module Sexpable with type t = 'a) -> ('a, Nothing.t) t
  val string : (module Stringable with type t = 'a) -> ('a, Nothing.t) t
  val invisible : ('a, Nothing.t) t
end

module Handle : sig
  type ('result, 'incoming) t

  (** [show] prints out the result of the component as specified by the [Result_spec] that
      was passed into [Handle.create]. *)
  val show : _ t -> unit

  (** [show_diff] will print the diff of the view between now and the last time that
      [show] or [show_diff] was called.

      [diff_context] can be used to adjust the number of unchanged lines before and after
      the diffed content.  Defaults to [16] *)
  val show_diff
    :  ?location_style:Patdiff_kernel.Format.Location_style.t
    -> ?diff_context:int
    -> _ t
    -> unit

  (** [recompute_view] is like [show], but it doesn't print anything. Calling
      [recompute_view] between invocations of [show_diff] does not affect the
      diff the gets shown. *)
  val recompute_view : _ t -> unit

  (** This function calls [recompute_view] until either
      [max_computes] is reached (defaults to 100), or there are no more
      after-display lifecycle events for processing.

      This can be useful when using e.g. [Bonsai.Edge.on_change], which might
      otherwise delay their effects until the next frame. *)
  val recompute_view_until_stable : ?max_computes:int -> _ t -> unit

  (** [store_view] is like [show] except that instead of printing the view to
      stdout, it only stores the current view for use with [show_diff].  This
      can be useful if you want to print the diff of "before->after" without
      being required to print the entirety of "before". *)
  val store_view : _ t -> unit

  val last_result : ('result, _) t -> 'result
  val do_actions : (_, 'incoming) t -> 'incoming list -> unit
  val clock : _ t -> Bonsai.Time_source.t
  val advance_clock_by : _ t -> Time_ns.Span.t -> unit
  val advance_clock : to_:Time_ns.t -> _ t -> unit

  val create
    :  ?start_time:Time_ns.t
    -> ?optimize:bool
    -> ('result, 'incoming) Result_spec.t
    -> 'result Computation.t
    -> ('result, 'incoming) t

  val show_model : _ t -> unit
    [@@alert
      rampantly_nondeterministic
        "This function exposes Bonsai internals that may change without warning"]

  val result_incr : ('r, 'i) t -> 'r Incr.t
  val lifecycle_incr : _ t -> Incr.Packed.t
  val action_input_incr : _ t -> Incr.Packed.t
  val has_after_display_events : ('result, 'incoming) t -> bool
  val print_actions : _ t -> unit
  val print_stabilizations : _ t -> unit
  val print_stabilization_tracker_stats : _ t -> unit
end
