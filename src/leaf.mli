open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { apply_action :
          inject:('action -> 'event)
          -> schedule_event:('event -> unit)
          -> 'input
          -> 'model
          -> 'action
          -> 'model
      ; compute : inject:('action -> 'event) -> 'input -> 'model -> 'result
      ; name : string
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked

val leaf
  :  (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> name:string
  -> default_model:'model
  -> apply_action:(inject:('action -> 'event)
                   -> schedule_event:('event -> unit)
                   -> 'input
                   -> 'model
                   -> 'action
                   -> 'model)
  -> compute:(inject:('action -> 'event) -> 'input -> 'model -> 'result)
  -> ('input, 'result, 'incr, 'event) Packed.t

val state_machine
  :  (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> Source_code_position.t
  -> default_model:'model
  -> apply_action:(inject:('action -> 'event)
                   -> schedule_event:('event -> unit)
                   -> 'input
                   -> 'model
                   -> 'action
                   -> 'model)
  -> ('input, 'model * ('action -> 'event), 'incr, 'event) Packed.t
