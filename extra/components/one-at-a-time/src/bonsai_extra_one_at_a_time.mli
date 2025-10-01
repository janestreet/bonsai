open! Core
module Bonsai := Bonsai.Cont
module Ui_effect := Bonsai.Effect

module Status : sig
  type t =
    | Busy
    | Idle
  [@@deriving sexp]
end

module Response : sig
  type 'a t =
    | Result of 'a
    | Exn of Exn.t
    | Busy
  [@@deriving sexp_of]
end

(** Turns the input effect into an effect which ensures that only one instance of it is
    running at a time. If another instance of the effect is already running, then [Busy]
    is returned instead of running the effect. In addition, it also returns a value
    representing whether or not an instance of the effect is in progress. *)
val effect
  :  ('query -> 'response Ui_effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> ('query -> 'response Response.t Ui_effect.t) Bonsai.t * Status.t Bonsai.t
