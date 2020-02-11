open! Core_kernel

module type Event_handler = sig
  module Event : T

  type 'action t

  val make_state : action_type_id:'action Type_equal.Id.t -> 'action t
  val iter_actions : 'action t -> f:('action -> unit) -> unit
  val schedule_event : _ t -> Event.t -> unit
  val inject : _ t -> Packed_action.t -> Event.t
end

module type S = sig
  module Bonsai : Bonsai.S

  type ('i, 'm, 'r) t

  val create
    :  initial_input:'i
    -> initial_model:'m
    -> ('i, 'm, 'r) Bonsai.t
    -> ('i, 'm, 'r) t

  val schedule_event : _ t -> Bonsai.Event.t -> unit

  (** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
  val flush : ('i, 'm, 'r) t -> unit

  val model : (_, 'm, _) t -> 'm
  val set_model : (_, 'm, _) t -> 'm -> unit
  val set_input : ('i, _, _) t -> 'i -> unit
  val result : (_, _, 'r) t -> 'r
end

module type Driver = sig
  module type Event_handler = Event_handler
  module type S = S

  module Make (Bonsai : Bonsai.S) (E : Event_handler with module Event = Bonsai.Event) :
    S with module Bonsai = Bonsai
end
