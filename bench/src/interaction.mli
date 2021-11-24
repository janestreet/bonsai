open! Core
open! Bonsai
open! Bonsai_test

type 'action t =
  | Change_input : 'a Var.t * 'a -> _ t
  | Inject : 'action -> 'action t
  | Advance_clock_by : Time_ns.Span.t -> _ t
  | Stabilize : _ t
  | Reset_model : _ t
  | Many : 'action t list -> 'action t

val advance_clock_by : Time_ns.Span.t -> _ t
val change_input : 'a Var.t -> 'a -> _ t
val inject : 'action -> 'action t
val stabilize : _ t
val reset_model : _ t
val many : 'action t list -> 'action t
val many_with_stabilizations : 'action t list -> 'action t
