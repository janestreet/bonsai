open! Core
open! Import
module Constants = Constants
module Underlying = Underlying_intf

module type Impl_s = sig
  include Underlying.S

  val singleton : c
end

type t = (module Impl_s)

val override_constants : t -> f:(Constants.t -> Constants.t) -> t
val name : t -> string
