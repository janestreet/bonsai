open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** Extends a Bonsai.t by providing a setter effect that can be used to override the
    returned value. The computation will initially evaluate to the input [Bonsai.t]. Once
    the returned overriding effect is dispatched at least once, the computation will
    evaluate to the override value provided. The effect can be scheduled more than once to
    update the override. Use with [Bonsai.with_model_resetter] in order to revert the
    override. *)
val value_with_override
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> 'a Bonsai.t * ('a -> unit Effect.t) Bonsai.t

(** Like [Bonsai.cutoff] but where the [equal] function can be determined dynamically. *)
val dynamic_cutoff
  :  'a Bonsai.t
  -> equal:('a -> 'a -> bool) Bonsai.t
  -> Bonsai.graph
  -> 'a Bonsai.t
