open! Core

type ('action, 'response) t

val inject : ('action, 'response) t -> 'action -> 'response Ui_effect.t
val schedule_event : _ t -> unit Ui_effect.t -> unit

(** Receive the time-source for this application. This is most likely useful to:
    - read the current time
    - sleep for some time span *)
val time_source : _ t -> Ui_time_source.t

module Private : sig
  type ('a, 'b) public_t := ('a, 'b) t

  type ('action, 'response) t =
    { inject : 'action -> 'response Ui_effect.t
    ; schedule_event : unit Ui_effect.t -> unit
    ; time_source : Ui_time_source.t
    }

  val reveal : ('a, 'b) public_t -> ('a, 'b) t

  val create
    :  inject:('action -> 'response Ui_effect.t)
    -> schedule_event:(unit Ui_effect.t -> unit)
    -> time_source:Ui_time_source.t
    -> ('action, 'response) public_t
end
