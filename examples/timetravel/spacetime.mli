open! Core
open Bonsai_web

module Model : sig
  type 'model t

  val create : 'model -> 'model t
end

module Action : sig
  type 'action t
end

module Result : sig
  (** The result of a timetravel component is a function which you can apply to
      some Vdom node in order to wrap the timetravel UI around it.

      The wrapped Vdom node does not have to correspond to the component passed to
      [create].  For example, that component might be part of a yet larger component, but
      you might want to wrap the outer UI with the timetravel UI. *)
  type t = Vdom.Node.t -> Vdom.Node.t
end

val create : ('input, 'result) Bonsai.t -> ('input, 'result * Result.t) Bonsai.t
