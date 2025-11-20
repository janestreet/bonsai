open! Core
module Incr = Ui_incr

module Timeable_event = struct
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
    | Profiling_entry of string
end

(* It's important that this is a function, not a global, because we want to create
   separate incremental nodes for each test handle / driver creation. *)
let default_for_test_handles () =
  { Bonsai.Private.Instrumentation.Config.instrument_for_computation_watcher =
      Incr.return Bonsai.Private.Instrumentation.Watching.Not_watching
  ; instrument_for_profiling =
      Incr.return Bonsai.Private.Instrumentation.Profiling.Not_profiling
  ; set_latest_graph_info = (fun _ -> ())
  ; computation_watcher_queue = Queue.create ()
  ; start_timer = (fun _ -> ())
  ; stop_timer = (fun () -> ())
  }
;;
