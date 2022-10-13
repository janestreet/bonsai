open! Core
open! Import
module Constants = Constants
module Underlying = Underlying_intf

module type Impl_s = sig
  include Underlying.S

  val singleton : c
end

type t = (module Impl_s)

let override_constants (theme : t) ~(f : Constants.t -> Constants.t) : t =
  let module T = (val theme) in
  let prev_constants = T.singleton#constants in
  let new_constants = f prev_constants in
  (module struct
    class c =
      object
        inherit T.c
        method! constants = new_constants
      end

    let singleton = new c
  end)
;;

let name ((module T) : t) = T.singleton#theme_name
