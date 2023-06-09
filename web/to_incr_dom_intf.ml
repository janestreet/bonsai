open! Core
open! Async_kernel
open Bonsai.For_open
open! Import

module type S = sig
  module Input : T

  module Model : sig
    type t [@@deriving equal, sexp_of]

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
    -> inject:(Action.t -> unit Vdom.Effect.t)
    -> t Incr.t
end

module type To_incr_dom = sig
  (** A wrapper to use Bonsai components in Incr_dom apps. *)
  module type S = S

  val convert
    :  ?optimize:bool
    -> ('input Value.t -> Vdom.Node.t Computation.t)
    -> (module S with type Input.t = 'input and type Extra.t = unit)

  val convert_with_extra
    :  ?optimize:bool
    -> ('input Value.t -> (Vdom.Node.t * 'extra) Computation.t)
    -> (module S with type Input.t = 'input and type Extra.t = 'extra)
end
