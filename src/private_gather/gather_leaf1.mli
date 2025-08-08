open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  model:'model Meta.Model.t
  -> input_id:'input Meta.Input.t
  -> dynamic_action:'action Type_equal.Id.t
  -> input:'input Value.t
  -> time_source:Time_source.t
  -> reset:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'model
        -> 'model)
  -> apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'input option
        -> 'model
        -> 'action
        -> 'model)
  -> here:Source_code_position.t
  -> ('model * ('action -> unit Effect.t), unit) Computation.packed_info Trampoline.t
