open! Core_kernel
open! Async_kernel
open! Import

module type Conv = sig
  type t

  val of_string : string -> t
  val to_string_hum : t -> string
  val name : string
end

module type S = sig
  type conv

  module Model = String

  include
    Bonsai.S
    with type Input.t = unit
    with module Model := Model
    with type Result.t = conv Or_error.t * Vdom.Node.t
end

module type String_input = sig
  module type Conv = Conv
  module type S = S

  module Make (Conv : Conv) : S with type conv := Conv.t
end
