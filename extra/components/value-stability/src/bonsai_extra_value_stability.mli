open! Core
module Bonsai := Bonsai.Cont

(** [with_last_modified_time] applies a cutoff to the input value and takes a note of the
    last time the value did not cutoff (in other words, the last time it was changed).

    Whenever the returned computation is activated, the "last time modified" value will be
    reset to the current time. *)
val with_last_modified_time
  :  equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> local_ Bonsai.graph
  -> 'a Bonsai.t * Time_ns.t Bonsai.t

(** [is_stable] indicates whether the input value has changed (according to [equal]) in
    the past specified time span. *)
val is_stable
  :  equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> time_to_stable:Time_ns.Span.t Bonsai.t
  -> local_ Bonsai.graph
  -> bool Bonsai.t

val most_recent_value_satisfying
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> condition:('a -> bool)
  -> local_ Bonsai.graph
  -> 'a option Bonsai.t

module Stability : sig
  type 'a t =
    | Stable of 'a
    | Unstable of
        { previously_stable : 'a option
        ; unstable_value : 'a
        }
  [@@deriving sexp, equal]

  val most_recent_stable_value : 'a t -> 'a option

  (** Return the [most_recent_stable_value] if there is one, otherwise fall back to the
      [unstable_value].f *)
  val prefer_stable_value : 'a t -> 'a
end

(** [value_stability] determines whether the current value has changed recently, and also
    keeps track of the most recent stable value. *)
val value_stability
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> time_to_stable:Time_ns.Span.t Bonsai.t
  -> local_ Bonsai.graph
  -> 'a Stability.t Bonsai.t
