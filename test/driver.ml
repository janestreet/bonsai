open! Core
open! Import

type ('i, 'r) t =
  { input_var : 'i Incr.Var.t
  ; mutable last_view : string
  ; handle : 'r Bonsai_driver.t
  }

let create
  (type i r)
  ?(optimize = true)
  ~clock
  ~(initial_input : i)
  (component : (i, r) Bonsai.Arrow_deprecated.t)
  : (i, r) t
  =
  let input_var, computation =
    let input_var = Incr.Var.create initial_input in
    let computation =
      input_var
      |> Incr.Var.watch
      |> Bonsai.Private.Value.of_incr
      |> Bonsai.Private.conceal_value
      |> component
    in
    input_var, computation
  in
  let handle = Bonsai_driver.create ~optimize ~clock computation in
  { input_var; last_view = ""; handle }
;;

let set_input { input_var; _ } input = Incr.Var.set input_var input
let input { input_var; _ } = Incr.Var.value input_var
let last_view { last_view; _ } = last_view
let store_view unpacked s = unpacked.last_view <- s
let reroute f { handle; _ } = f handle

include struct
  open Bonsai_driver

  let schedule_event h = reroute schedule_event h
  let flush h = reroute flush h
  let result h = reroute result h
  let has_after_display_events h = reroute has_after_display_events h
  let trigger_lifecycles h = reroute trigger_lifecycles h
end

include struct
  open Bonsai_driver.Expert

  let sexp_of_model h = reroute sexp_of_model h
  let result_incr h = reroute result_incr h
  let action_input_incr h = reroute action_input_incr h
  let lifecycle_incr h = reroute lifecycle_incr h
  let clock h = reroute clock h
  let invalidate_observers h = reroute invalidate_observers h
  let reset_model_to_default h = reroute reset_model_to_default h
  let print_actions h = reroute print_actions h
  let print_stabilizations h = reroute print_stabilizations h
  let print_stabilization_tracker_stats h = reroute print_stabilization_tracker_stats h
end
