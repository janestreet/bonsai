open! Core
open Bonsai.For_open

module Interpolator : sig
  type t =
    | Linear
    | Ease_in_quad
    | Ease_out_quad
    | Ease_in_out_quad
    | Ease_in_cubic
    | Ease_out_cubic
    | Ease_in_out_cubic
    | Ease_in_quart
    | Ease_out_quart
    | Ease_in_out_quart
    | Ease_in_quint
    | Ease_out_quint
    | Ease_in_out_quint
    | Ease_in_sin
    | Ease_out_sin
    | Ease_in_out_sin
    | Ease_in_exp
    | Ease_out_exp
    | Ease_in_out_exp
    | Ease_in_circ
    | Ease_out_circ
    | Ease_in_out_circ
    | Ease_in_back
    | Ease_out_back
    | Ease_in_out_back
  [@@deriving sexp, equal, compare, enumerate]
end

module Interpolatable : sig
  type 'a t = 'a -> 'a -> float -> 'a

  val float : float t
  val int : int t
end

(* Smooths out the value of ['a] as it changes inside its containing Value.t.
   When the ['a] changes, the [interpolate] method is used to in-between the
   value to the new value.  *)
val smooth
  :  ?sexp_of_model:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> ?with_:Interpolator.t
  -> duration:Time_ns.Span.t Value.t
  -> interpolate:'a Interpolatable.t
  -> 'a Value.t
  -> 'a Computation.t

module Advanced : sig
  type time_spec :=
    [ `End_at of Time_ns.t
    | `For of Time_ns.Span.t
    | `Now
    ]

  type 'a t =
    { value : 'a
    ; animate :
        ?after_finished:unit Effect.t
        -> ?with_:Interpolator.t
        -> time_spec
        -> 'a
        -> unit Effect.t
    }

  val make : fallback:'a Value.t -> interpolate:'a Interpolatable.t -> 'a t Computation.t
end
