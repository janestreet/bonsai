open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** As its name implies, [exactly_once] runs the event passed in via [Bonsai.t] exactly
    once. *)
val exactly_once : unit Effect.t Bonsai.t -> local_ Bonsai.graph -> unit

(** As its name implies, [exactly_once] runs the event passed in via [Bonsai.t] exactly
    once. The return value is stored and returned. [None] is returned while the effect is
    executing. *)
val exactly_once_with_value
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> 'a Effect.t Bonsai.t
  -> local_ Bonsai.graph
  -> 'a option Bonsai.t

(** [bonk] returns a function which takes an effect and transforms it into the same
    effect, but it will be enqueued onto the Bonsai action queue instead of running
    immediately. This can be useful in niche situations where you have a batch of effects
    that would otherwise interleave and have bad performance behavior. *)
val bonk : local_ Bonsai.graph -> (unit Effect.t -> unit Effect.t) Bonsai.t

(** [chain_incr_effects input effects] allows you to sequentially schedule effects that
    depend on a common ['a Bonsai.t], while ensuring that no effect will receive a stale
    input. This function short-circuits if the input becomes inactive while actions are
    still being applied.

    This is particularly useful for modeling a set of interacting state machines. The
    outputs of each computation can be collected into a single [Bonsai.t], which is then
    provided to each state machine through an injected action. This util allows model
    recomputations made in the `i`th state machine to be immediately visible to the
    [apply_action] logic of the `i+1`th state machine.

    In contrast, just resolving a value with [let%arr] and scheduling multiple dependent
    effects with `[Effect.Many]` will provide all state machines with the state of the
    world before *any* of them recalculated state. *)
val chain_incr_effects
  :  'a Bonsai.t
  -> local_ Bonsai.graph
  -> (('a -> unit Ui_effect.t) list -> unit Ui_effect.t) Bonsai.t
