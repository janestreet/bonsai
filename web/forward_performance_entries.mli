open! Core
open! Bonsai

type ('model, 'static_action, 'dynamic_action, 'result) t =
  { instrumented_computation :
      ('model, 'static_action, 'dynamic_action, 'result) Bonsai.Private.Computation.t
  ; shutdown : unit -> unit
  }

(** Instruments a computation and and loads a web worker from the specified
    host and port. The worker is responsible for sending data to the debugger/profiler *)

val instrument
  :  host:string
  -> port:int
  -> worker_name:string
  -> ('model, 'static_action, 'dynamic_action, 'result) Bonsai.Private.Computation.t
  -> ('model, 'static_action, 'dynamic_action, 'result) t
