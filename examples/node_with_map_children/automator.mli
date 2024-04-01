open! Core
open! Bonsai_web.Cont

(** Given values and effects for manipulating the other components in
    the app, [Automator.component] will run through testing scenarios,
    pausing after every frame to see if the results can be validated. *)

val component
  :  is_running:bool Bonsai.t
  -> reset_all:unit Effect.t Bonsai.t
  -> step:unit Effect.t Bonsai.t
  -> is_done:bool Bonsai.t
  -> Bonsai.graph
  -> unit Bonsai.t
