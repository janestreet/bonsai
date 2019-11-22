open! Core_kernel
open! Import
include Snapshot_intf

module Make (Event : T) = struct
  type ('model, 'action, 'result) t =
    { apply_action : schedule_event:(Event.t -> unit) -> 'action -> 'model
    ; result : 'result
    }
  [@@deriving fields]

  let create = Fields.create
end
