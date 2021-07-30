open! Core
open! Import

type ('model, 'action, 'result) t =
  { apply_action : (schedule_event:(unit Effect.t -> unit) -> 'action -> 'model) Incr.t
  ; lifecycle : Lifecycle.Collection.t Incr.t option
  ; result : 'result Incr.t
  }
[@@deriving fields]

let create = Fields.create

let lifecycle_or_empty t =
  lifecycle t |> Option.value ~default:(Incr.return Lifecycle.Collection.empty)
;;
