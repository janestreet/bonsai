open! Core
open! Import

(** A clock that supports both incremental operations and also alarms. *)
type t

(** Creates a new clock starting at the specified time. *)
val create : start:Time_ns.t -> t

(** Pulls out the Incremental clock within the Bonsai clock. You should avoid
    this function if possible; if the interface in this module does not have an
    incremental function you need, then we can add it. *)
val incr_clock : t -> Incr.Clock.t

(** Moves the current time forward by a time span; any alarms are enqueued instead of triggered. *)
val advance_clock_by : t -> Time_ns.Span.t -> unit

(** Moves the current time forward to a specific instant; any alarms are enqueued instead of triggered. *)
val advance_clock : t -> to_:Time_ns.t -> unit

(** The current time. *)
val now : t -> Time_ns.t

(** An incremental view on the current time. *)
val watch_now : t -> Time_ns.t Incr.t

(** A unit value that will trigger every specified time span. *)
val at_intervals : t -> Time_ns.Span.t -> unit Incr.t

(** An value that switches from [Before] to [After] at the specified instant. *)
val at : t -> Time_ns.t -> Incr.Before_or_after.t Incr.t

(** An effect that waits to complete until the clock reaches the specified instant. *)
val until : t -> Time_ns.t -> unit Effect.t

(** An effect that waits to complete until the clock advances by the specified time span. *)
val sleep : t -> Time_ns.Span.t -> unit Effect.t

module Private : sig
  (** Users should not need to call this function, since it is managed by
      Bonsai. Triggers any alarms that have been enqueued. *)
  val flush : t -> unit
end
