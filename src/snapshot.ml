open! Core_kernel
open! Import

type ('model, 'action, 'result) t =
  { apply_action : (schedule_event:(Event.t -> unit) -> 'action -> 'model) Incr.t
  ; after_display : (schedule_event:(Event.t -> unit) -> unit) option Incr.t
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create = Fields.create
