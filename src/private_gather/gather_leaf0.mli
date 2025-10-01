open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  model:'model Meta.Model.t
  -> static_action:'action Type_equal.Id.t
  -> time_source:Time_source.t
  -> apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'model
        -> 'action
        -> 'model)
  -> reset:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'model
        -> 'model)
  -> here:Source_code_position.t
  -> ('model * ('action -> unit Effect.t), unit) Computation.packed_info Trampoline.t
