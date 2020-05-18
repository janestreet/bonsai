open! Core_kernel
open! Async_kernel
open! Bonsai_web

module Input : sig
  type t

  val create : send_message:(string -> unit Effect.t) -> t
end

module Model : sig
  type t

  val default : t
end

val component : (Input.t, Vdom.Node.t) Bonsai.t
