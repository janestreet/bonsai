open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { apply_action :
          ('input, 'incr) Incremental.t
          -> ('model, 'incr) Incremental.t
          -> inject:('action -> 'event)
          -> (schedule_event:('event -> unit) -> 'action -> 'model, 'incr) Incremental.t
      ; compute :
          ('input, 'incr) Incremental.t
          -> ('model, 'incr) Incremental.t
          -> inject:('action -> 'event)
          -> ('result, 'incr) Incremental.t
      ; name : string
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
