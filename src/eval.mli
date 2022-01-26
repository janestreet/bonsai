open! Core
open! Import

val eval
  :  environment:Environment.t
  -> path:Path.t
  -> clock:Incr.Clock.t
  -> model:'model Incr.t
  -> inject_dynamic:('dynamic_action -> unit Effect.t)
  -> inject_static:('static_action -> unit Effect.t)
  -> ('model, 'dynamic_action, 'static_action, 'result) Computation.t
  -> ('model, 'dynamic_action, 'result) Snapshot.t
