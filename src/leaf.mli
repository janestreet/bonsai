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
