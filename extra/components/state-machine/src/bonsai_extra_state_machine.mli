open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** This function is identical to [Bonsai.state_machine0] except that the [default_model]
    is initially unset, but can be computed or defaulted to a dynamic value.

    This means that before an apply_action occurs, the model can change out from
    underneath the state machine as the default_model value changes. If this is
    undesirable, you may want to [freeze] the default_model first. *)
val state_machine0_dynamic_model
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> local_ Bonsai.graph
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t

(** Read the docs for [state_machine0_dynamic_model]. This one has an extra ['input] value
    that can be taken into account when the [apply_action] is invoked. *)
val state_machine1_dynamic_model
  :  ?sexp_of_action:('action -> Sexp.t)
  -> ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t
        -> 'input
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Bonsai.t
  -> local_ Bonsai.graph
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t

(** The analog of [Bonsai.state], but with a dynamic model. Read the docs for
    [state_machine0_dynamic_model].

    When passed a [`Given] model, behaves like [value_with_override]; takes on the initial
    value of the given model and has its value overwritten when the callback is scheduled.

    When passed a [`Computed f] model, takes initial value [f None]. When the callback is
    scheduled with input [i], takes the value [f i]. *)
val state_dynamic_model
  :  ?sexp_of_model:('model -> Sexp.t)
  -> ?equal:('model -> 'model -> bool)
  -> model:
       [< `Computed of ('model option -> 'model) Bonsai.t | `Given of 'model Bonsai.t ]
  -> local_ Bonsai.graph
  -> 'model Bonsai.t * ('model -> unit Effect.t) Bonsai.t
