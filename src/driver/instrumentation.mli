open! Core
module Incr = Ui_incr

module Timeable_event : sig
  (** These are fairly arbitrary, and are subject to change. *)
  type t =
    (* Startup *)
    | Graph_application
    | Preprocess
    | Gather
    | Run_eval_fun
    | First_stabilization
    (* Per-frame *)
    | Stabilize_for_clock
    | Apply_actions
    | Stabilize_for_action
    | Stabilize_after_all_apply_actions
    (* When profiling *)
    | Profiling_entry of Bonsai.Private.Instrumentation.Profiled_event.t
  [@@deriving equal, sexp_of]
end

val default_for_test_handles : unit -> (_, unit) Bonsai.Private.Instrumentation.Config.t

module Packed : sig
  type t = T : (Timeable_event.t, 'timer) Bonsai.Private.Instrumentation.Config.t -> t
end
