open! Core
open! Bonsai

type ('a, 'r) unpacked =
  { clock : Ui_incr.Clock.t
  ; name : string
  ; component : 'r Computation.t
  ; get_inject : 'r -> 'a -> unit Effect.t
  ; interaction : 'a Interaction.t
  }

type t = T : (_, _) unpacked -> t

val create
  :  ?clock:Ui_incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

val create_with_resetter
  :  ?clock:Ui_incr.Clock.t
  -> name:string
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t
