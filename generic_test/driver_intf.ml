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

  type ('i, 'r) t

  val create
    :  ?initial_model_sexp:Sexp.t
    -> initial_input:'i
    -> ('i, 'r) Bonsai.t
    -> ('i, 'r) t

  val schedule_event : _ t -> Bonsai.Event.t -> unit

  (** Apply all pending actions and stabilize the incremental graph, updating [result]. *)
  val flush : _ t -> unit

  val set_input : ('i, _) t -> 'i -> unit
  val input : ('i, _) t -> 'i
  val result : (_, 'r) t -> 'r
  val sexp_of_model : (_, _) t -> Sexp.t
end

module type Driver = sig
  module type Event_handler = Event_handler
  module type S = S

  module Make (Bonsai : Bonsai.S) (E : Event_handler with module Event = Bonsai.Event) :
    S with module Bonsai = Bonsai
end
