open! Core_kernel
open! Async_kernel
open! Import

module type S = sig
  module Input : T

  module Model : sig
    type t [@@deriving equal, sexp]

    val default : t
  end

  module Extra : T

  module Action : sig
    type t [@@deriving sexp_of]
  end

  module State : sig
    type t

    val create : unit -> t
  end

  type t = (Action.t, Model.t, State.t, Extra.t) Incr_dom.Component.with_extra

  val create
    :  input:Input.t Incr.t
    -> old_model:Model.t option Incr.t
    -> model:Model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> t Incr.t
end

module type To_incr_dom = sig
  (** A wrapper to use Bonsai components in Incr_dom apps. *)
  module type S = S

  val convert
    :  ('input Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t)
    -> (module S with type Input.t = 'input and type Extra.t = unit)

  val convert_with_extra
    :  ('input Bonsai.Value.t -> (Vdom.Node.t * 'extra) Bonsai.Computation.t)
    -> (module S with type Input.t = 'input and type Extra.t = 'extra)
end
