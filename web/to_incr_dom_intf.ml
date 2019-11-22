open! Core_kernel
open! Async_kernel
open! Import

module type S = sig
  module Input : T
  module Model : T
  module Extra : T

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  input:Input.t Incr.t
    -> old_model:Model.t option Incr.t
    -> model:Model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> (Action.t, Model.t, unit, Extra.t) Incr_dom.Component.with_extra Incr.t
end

module type To_incr_dom = sig
  (** A wrapper to use Bonsai components in Incr_dom apps. *)

  module type S = S

  val convert
    :  ('input, 'model, Vdom.Node.t) Bonsai.t
    -> (module S
         with type Model.t = 'model
          and type Input.t = 'input
          and type Extra.t = unit)

  val convert_with_extra
    :  ('input, 'model, Vdom.Node.t * 'extra) Bonsai.t
    -> (module S
         with type Model.t = 'model
          and type Input.t = 'input
          and type Extra.t = 'extra)
end
