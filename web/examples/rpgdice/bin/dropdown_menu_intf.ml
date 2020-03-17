open! Core_kernel
open! Async_kernel
open! Import

module type Enum = sig
  type t [@@deriving enumerate, equal, sexp]

  val to_string : t -> string
  val name : string
end

module type S = sig
  type enum

  include
    Bonsai.S
    with type Input.t = unit
    with type Model.t = enum
    with type Result.t = enum * Vdom.Node.t
end

module type Dropdown_menu = sig
  module type Enum = Enum
  module type S = S

  module Make (E : Enum) : S with type enum := E.t
end
