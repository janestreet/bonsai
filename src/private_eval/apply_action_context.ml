open! Core

type ('action, 'response) t =
  { inject : 'action -> 'response Ui_effect.t
  ; schedule_event : unit Ui_effect.t -> unit
  ; time_source : Ui_time_source.t
  }

let inject { inject; schedule_event = _; time_source = _ } action = inject action

let schedule_event { inject = _; schedule_event; time_source = _ } event =
  schedule_event event
;;

let time_source { inject = _; schedule_event = _; time_source } = time_source

module Private = struct
  type nonrec ('action, 'response) t = ('action, 'response) t =
    { inject : 'action -> 'response Ui_effect.t
    ; schedule_event : unit Ui_effect.t -> unit
    ; time_source : Ui_time_source.t
    }

  let reveal = Fn.id

  let create ~inject ~schedule_event ~time_source =
    { inject; schedule_event; time_source }
  ;;
end
