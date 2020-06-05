open! Core_kernel
open! Import

val eval
  :  Environment.t
  -> 'model Incr.t
  -> inject:('action -> Event.t)
  -> ('model, 'action, 'result) Computation.t
  -> ('model, 'action, 'result) Snapshot.t Incr.t
