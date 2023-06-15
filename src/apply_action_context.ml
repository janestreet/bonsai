open! Core

type 'action t =
  { inject : 'action -> unit Ui_effect.t
  ; schedule_event : unit Ui_effect.t -> unit
  }

let inject { inject; schedule_event = _ } action = inject action
let schedule_event { inject = _; schedule_event } event = schedule_event event
let create ~inject ~schedule_event = { inject; schedule_event }

module Private = struct
  type nonrec 'action t = 'action t =
    { inject : 'action -> unit Ui_effect.t
    ; schedule_event : unit Ui_effect.t -> unit
    }

  let reveal = Fn.id
end
