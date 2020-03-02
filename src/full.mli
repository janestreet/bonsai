open! Core_kernel
open! Import
open Component

type ('input, 'model, 'action, 'result, 'incr, 'event) t =
  input:('input, 'incr) Incremental.t
  -> old_model:('model option, 'incr) Incremental.t
  -> model:('model, 'incr) Incremental.t
  -> inject:('action -> 'event)
  -> incr_state:'incr Incremental.State.t
  -> (('model, 'action, 'result, 'event) Snapshot.t, 'incr) Incremental.t

type ('input, 'model, 'action, 'result, 'incr, 'event) unpacked +=
  | C :
      { f : ('input, 'model, 'action, 'result, 'incr, 'event) t
      ; constructed_at : Source_code_position.t
      }
      -> ('input, 'model, 'action, 'result, 'incr, 'event) unpacked
