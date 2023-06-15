open! Core

type 'action t

val inject : 'action t -> 'action -> unit Ui_effect.t
val schedule_event : _ t -> unit Ui_effect.t -> unit

val create
  :  inject:('action -> unit Ui_effect.t)
  -> schedule_event:(unit Ui_effect.t -> unit)
  -> 'action t

module Private : sig
  type 'action public_t := 'action t

  type 'action t =
    { inject : 'action -> unit Ui_effect.t
    ; schedule_event : unit Ui_effect.t -> unit
    }

  val reveal : 'action public_t -> 'action t
end
