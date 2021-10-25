open! Core
open! Bonsai

module Id : sig
  type t [@@deriving equal, sexp]

  include Comparable.S with type t := t
end

type 'a t =
  { contents : 'a Id.Map.t
  ; append : unit Ui_effect.t
  ; set_length : int -> unit Ui_effect.t
  ; remove : Id.t -> unit Ui_effect.t
  }

(** Given a computation, builds a new computation that can hold
    a dynamic number of the wrapped computation. *)
val component : Source_code_position.t -> 'a Computation.t -> 'a t Computation.t

(** Like [component], but with the power to extend the result of the
    input component with an event that removes itself. *)
val component'
  :  Source_code_position.t
  -> 'a Computation.t
  -> wrap_remove:('a -> unit Ui_effect.t -> 'b)
  -> 'b t Computation.t
