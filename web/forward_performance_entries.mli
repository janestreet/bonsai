open! Core

type 'result t =
  { instrumented_computation : 'result Bonsai.Private.Computation.t
  ; shutdown : unit -> unit
  }

(** Instruments a computation and loads a web worker from the specified
    host and port. The worker is responsible for sending data to the debugger/profiler *)

val instrument
  :  host:string
  -> port:int
  -> worker_name:string
  -> 'result Bonsai.Private.Computation.t
  -> 'result t
