open! Core
open! Bonsai_web

module type Enum = sig
  type t [@@deriving enumerate, equal, sexp]

  val to_string : t -> string
  val name : string
end

module type S = sig
  type enum

  val component : default_model:enum -> (enum * Vdom.Node.t) Computation.t
end

module type Dropdown_menu = sig
  module type Enum = Enum
  module type S = S

  module Make (E : Enum) : S with type enum := E.t
end
