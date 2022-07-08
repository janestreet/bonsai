open! Core
open! Bonsai
open! Bonsai_test

type ('a, 'r) unpacked =
  { clock : Incr.Clock.t
  ; name : string
  ; component : 'r Computation.t
  ; get_inject : 'r -> 'a -> unit Effect.t
  ; interaction : 'a Interaction.t
  }

type t = T : (_, _) unpacked -> t

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
