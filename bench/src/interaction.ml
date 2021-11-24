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

let change_input var value = Change_input (var, value)
let inject action = Inject action
let advance_clock_by span = Advance_clock_by span
let stabilize = Stabilize
let reset_model = Reset_model
let many ts = Many ts
let many_with_stabilizations ts = Many (List.intersperse ts ~sep:Stabilize)
