open! Core_kernel

type ('model, 'action, 'result, 'event) t =
  { apply_action : schedule_event:('event -> unit) -> 'action -> 'model
  ; result : 'result
  }
[@@deriving fields]

let create = Fields.create
