open! Core
open! Import

type ('model, 'action, 'result) t =
  { apply_action : (schedule_event:(Event.t -> unit) -> 'action -> 'model) Incr.t
  ; lifecycle : Lifecycle.Collection.t Incr.t
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create = Fields.create
