open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'result Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> wrapper_model:'model Meta.Model.t
  -> action_id:'action Type_equal.Id.t
  -> result_id:'result Meta.Input.t
  -> inject_id:('action -> unit Effect.t) Type_equal.Id.t
  -> model_id:'model Type_equal.Id.t
  -> inner:'result Computation.t
  -> dynamic_apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'result option
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
  -> ('result, unit) Computation.packed_info Trampoline.t
