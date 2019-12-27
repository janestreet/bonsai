open! Core_kernel
open! Async_kernel
open! Bonsai_web

module Input : sig
  type t

  val create : inject_send_message:(string -> Vdom.Event.t) -> t
end

module Model : sig
  type t

  val default : t
end

val component : (Input.t, Model.t, Vdom.Node.t) Bonsai.t
