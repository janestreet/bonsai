open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** Extends a Bonsai.t by providing a setter effect that can be used to override the
    returned value. The computation will initially evaluate to the input [Bonsai.t]. Once
    the returned overriding effect is dispatched at least once, the computation will
    evaluate to the override value provided. The effect can be scheduled more than once to
    update the override. Use with [Bonsai.with_model_resetter_n] in order to revert the
    override (see the corresponding test in test/test_cont.ml for an example). *)
val value_with_override
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a Bonsai.t
  -> Bonsai.graph @ local
  -> 'a Bonsai.t * ('a -> unit Effect.t) Bonsai.t

(** Extends a [Bonsai.state_machine] (value/setter pair) with a dynamic "default model".
    The value will initially evaluate to the [default_model], but once [apply_action] is
    dispatched at least once when [default_model] is active, it will reflect the updated
    model. *)
val value_with_state_machine_override
  :  here:[%call_pos]
  -> ?reset:(('action, unit) Bonsai.Apply_action_context.t -> 'model -> 'model)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?sexp_of_action:('action -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> default_model:'model Bonsai.t
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> Bonsai.graph @ local
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t

(** Like [Bonsai.cutoff] but where the [equal] function can be determined dynamically. *)
val dynamic_cutoff
  :  'a Bonsai.t
  -> equal:('a -> 'a -> bool) Bonsai.t
  -> Bonsai.graph @ local
  -> 'a Bonsai.t
