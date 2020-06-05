open! Core_kernel
open! Import

type ('model, 'action, 'result) t =
  { apply_action : schedule_event:(Event.t -> unit) -> 'action -> 'model
  ; result : 'result
  }
[@@deriving fields]

let create = Fields.create
