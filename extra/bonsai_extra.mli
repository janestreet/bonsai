open! Core
open! Bonsai

val map_of_set : ('k, 'cmp) Set.t Value.t -> ('k, unit, 'cmp) Map.t Computation.t
val map_keys : ('k, _, 'cmp) Map.t Value.t -> ('k, 'cmp) Set.t Computation.t

(** [with_inject_fixed_point] allows an injection function produced as the
    result of a computation to be used as the input of that same combination.
    This "tie-the-knot" operation is legal because actions are scheduled
    independently of computation evaluation, allowing us to break what seems
    like a dependency loop.

    However, it is important that the input injection function isn't the same
    one that is returned by the computation (or even a component of the returned
    injection function).  If that happens, an action being triggered will cause
    an inifinite loop to occur in the aciton scheduler. *)
val with_inject_fixed_point
  :  (('action -> unit Effect.t) Value.t
      -> ('result * ('action -> unit Effect.t)) Computation.t)
  -> 'result Computation.t

(** [yoink] is a function that takes a bonsai value and produces a
    computation producing an effect which fetches the current value out of the
    input.  This can be useful inside of [let%bind.Effect] chains, where a
    value that you've closed over is stale and you want to witness a value
    after it's been changed by a previous effect. *)
val yoink : 'a Value.t -> 'a Effect.t Computation.t

(** [pipe] constructs a pipe of [a] and returns a pair containing an injection
    function that enqueues items and an Effect that dequeues them.  *)
val pipe
  :  Source_code_position.t
  -> (module Bonsai.Model with type t = 'a)
  -> (('a -> unit Ui_effect.t) * 'a Effect.t) Bonsai.Computation.t

(** As its name implies, [exactly_once] runs the event passed in via [Value.t]
    exactly once. *)
val exactly_once
  :  Source_code_position.t
  -> unit Ui_effect.t Value.t
  -> unit Computation.t

(** [freeze] takes a Value.t and returns a computation whose output is frozen
    to be the first value that passed through the input. *)
val freeze
  :  Source_code_position.t
  -> (module Model with type t = 'a)
  -> 'a Value.t
  -> 'a Computation.t

(* [thunk] will execute its argument exactly once per instantiation of the 
   computation. *)
val thunk : (unit -> 'a) -> 'a Computation.t

(** This function is identical to [Bonsai.state_machine0] except that
    the [default_model] is initially unset, but can be computed or defaulted
    to a dynamic value.

    This means that the model can change out from underneath the state machine
    as the default_model value changes.  If this is undesirable, you may want
    to [freeze] the default_model first. *)
val state_machine0_dynamic_model
  :  Source_code_position.t
  -> (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> model:[< `Computed of ('model option -> 'model) Value.t | `Given of 'model Value.t ]
  -> apply_action:
       (inject:('action -> unit Ui_effect.t)
        -> schedule_event:(unit Ui_effect.t -> unit)
        -> 'model
        -> 'action
        -> 'model)
  -> ('model * ('action -> unit Ui_effect.t)) Computation.t

(** Read the docs for [state_machine0_dynamic_model]. This one has
    an extra ['input] value that can be taken into account when the
    [apply_action] is invoked. *)
val state_machine1_dynamic_model
  :  Source_code_position.t
  -> (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> model:[< `Computed of ('model option -> 'model) Value.t | `Given of 'model Value.t ]
  -> apply_action:
       (inject:('action -> unit Ui_effect.t)
        -> schedule_event:(unit Ui_effect.t -> unit)
        -> 'input
        -> 'model
        -> 'action
        -> 'model)
  -> 'input Value.t
  -> ('model * ('action -> unit Ui_effect.t)) Computation.t

(** Id_gen builds a compoenent which generates unique identifiers by
    starting at 0 and incrementing by one every time that the effect is called.

    The functor is parameteraized on the size of integer (int vs int63 vs
    int64), and it's also a generative functor, so invoking the functor
    multiple times times will mint new types.

    I explicitly disassociate the input T from the output T because otherwise
    the benefits of the generative functor would be gone. *)
module Id_gen (T : Int_intf.S) () : sig
  include Int_intf.S

  val component : Source_code_position.t -> t Bonsai.Effect.t Bonsai.Computation.t
end
