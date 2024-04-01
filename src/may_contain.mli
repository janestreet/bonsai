open! Core

module type S := sig
  type t =
    | Yes_or_maybe
    | No

  val merge : t -> t -> t
end

module Lifecycle : S
module Path : S
