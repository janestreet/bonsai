open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** [pipe] constructs a pipe of [a] and returns a pair containing an injection function
    that enqueues items and an Effect that dequeues them. *)
val pipe
  :  ?sexp_of:('a -> Sexp.t)
  -> local_ Bonsai.graph
  -> ('a -> unit Effect.t) Bonsai.t * 'a Effect.t Bonsai.t
