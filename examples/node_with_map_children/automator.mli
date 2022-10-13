open! Core
open! Bonsai_web

(** Given values and effects for manipulating the other components in
    the app, [Automator.component] will run through testing scenarios,
    pausing after every frame to see if the results can be validated. *)

val component
  :  is_running:bool Value.t
  -> reset_all:unit Effect.t Value.t
  -> step:unit Effect.t Value.t
  -> is_done:bool Value.t
  -> unit Computation.t
