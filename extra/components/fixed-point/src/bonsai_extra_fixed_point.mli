open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** [with_inject_fixed_point] allows an injection function produced as the result of a
    computation to be used as the input of that same combination. This "tie-the-knot"
    operation is legal because actions are scheduled independently of computation
    evaluation, allowing us to break what seems like a dependency loop.

    However, it is important that the input injection function isn't the same one that is
    returned by the computation (or even a component of the returned injection function).
    If that happens, an action being triggered will cause an inifinite loop to occur in
    the action scheduler. *)
val with_inject_fixed_point
  :  (('action -> unit Effect.t) Bonsai.t
      -> local_ Bonsai.graph
      -> 'result Bonsai.t * ('action -> unit Effect.t) Bonsai.t)
  -> local_ Bonsai.graph
  -> 'result Bonsai.t

(** [with_self_effect] gives access to an effect which produces the output ['a] within the
    body of [f]. This can be useful if you'd like to install an effect handling function
    on [f] whose logic depends on its current value. *)
val with_self_effect
  :  ?sexp_of_model:('a -> Sexp.t)
  -> ?equal:('a -> 'a -> bool)
  -> f:
       ('a Bonsai.Computation_status.t Effect.t Bonsai.t
        -> local_ Bonsai.graph
        -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t
