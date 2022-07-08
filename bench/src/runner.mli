open Bonsai.For_open
open Bonsai_test

type t
type wrap_create = { f : 'a. (unit -> 'a) -> 'a } [@@unboxed]

val initialize
  :  filter_profiles:bool
  -> wrap_driver_creation:wrap_create
  -> clock:Incr.Clock.t
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> interaction:'a Interaction.t
  -> t

val run_interactions : t -> handle_profile:(string -> unit) -> unit
val invalidate_observers : t -> unit
