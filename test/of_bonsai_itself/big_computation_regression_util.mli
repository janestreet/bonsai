open! Core
open Bonsai_test

(** These are designed to test the generated path IDs, and shapes of [Computation.t]
    and [Incr.t]s produced by the proc and cont APIs.

    [basic] subs on a subcomputation [width] times, where the subcomputation is a chain
    of [height] unused [Bonsai.state]s.

    [with_assoc] subs on [basic], then creates an [Assoc] node with up to [assoc_size]
    inputs, with another call to [basic] as the contents.

    [with_switch] subs on [basic], then creates a [Switch] node with a constant arm,
    a simple `let%arr` arm, and an arm that's just [basic] again. *)

module For_proc : sig
  val basic : height:int -> width:int -> Core.String.Set.t Bonsai.Computation.t

  val with_assoc
    :  height:int
    -> width:int
    -> num_assocs:int
    -> Core.String.Set.t Core.String.Map.t Bonsai.Computation.t

  val with_switch : height:int -> width:int -> int Bonsai.Computation.t
end

module For_cont : sig
  module Bonsai = Bonsai.Cont

  val basic : height:int -> width:int -> Bonsai.graph -> Core.String.Set.t Bonsai.t

  val with_assoc
    :  height:int
    -> width:int
    -> num_assocs:int
    -> Bonsai.graph
    -> Core.String.Set.t Core.String.Map.t Bonsai.t

  val with_switch : height:int -> width:int -> Bonsai.graph -> int Bonsai.t
end

val lengths_result_spec
  : (module Result_spec.S
       with type incoming = never_returns
        and type t = Core.String.Set.t)

val values_result_spec
  : (module Result_spec.S
       with type incoming = never_returns
        and type t = Core.String.Set.t)
