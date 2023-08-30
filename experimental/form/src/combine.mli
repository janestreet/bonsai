open! Core
open! Import
open Bonsai_web

(** Lift the setter function in the [Product.t] of a component. This is used to align the
    ['parsed] types which is needed to use Applicative and Let_syntax. *)
val lift
  :  f:('parsed2 -> 'parsed1)
  -> ('input, ('result, 'parsed1) Product.t) Bonsai.Arrow_deprecated.t
  -> ('input, ('result, 'parsed2) Product.t) Bonsai.Arrow_deprecated.t

module Open_on_rhs_intf : sig
  module type S = sig
    val lift
      :  f:('parsed2 -> 'parsed1)
      -> ('input, ('result, 'parsed1) Product.t) Bonsai.Arrow_deprecated.t
      -> ('input, ('result, 'parsed2) Product.t) Bonsai.Arrow_deprecated.t
  end
end

include
  Applicative.S3
    with type ('result, 'input, 'parsed) t :=
      ('input, ('result, 'parsed) Product.t) Bonsai.Arrow_deprecated.t

include
  Applicative.Let_syntax3
    with type ('result, 'input, 'parsed) t :=
      ('input, ('result, 'parsed) Product.t) Bonsai.Arrow_deprecated.t
    with module Open_on_rhs_intf := Open_on_rhs_intf
