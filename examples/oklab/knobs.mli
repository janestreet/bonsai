open! Core
open! Bonsai_web.Cont

module Shared : sig
  type t =
    { left : [ `Hex of string ]
    ; right : [ `Hex of string ]
    }
end

module For_gradient : sig
  type t = { steps : int } [@@deriving typed_fields]
end

module For_overlay : sig
  type t =
    { left_alpha : float
    ; right_alpha : float
    }
end

type t =
  { shared : Shared.t
  ; for_gradient : For_gradient.t
  ; for_overlay : For_overlay.t
  }

val form : Bonsai.graph -> (t * Vdom.Node.t) Bonsai.t
