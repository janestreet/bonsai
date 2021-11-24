open! Core
open! Bonsai
open! Bonsai_test

type t = Core_bench_js.Test.t

val create
  :  ?clock:Incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

val create_with_resetter
  :  ?clock:Incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t
