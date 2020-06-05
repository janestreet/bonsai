open! Core_kernel
open! Import
include module type of Legacy_api

module Proc :
  module type of Proc
  (*_ The following line has the effect of hiding the Private module from the public
    API.  The values therein are exposed in {!Private} below. *)
  with module Private := Proc.Private

module Private : sig
  module Snapshot = Snapshot
  module Environment = Environment
  module Computation = Computation
  module Value = Value

  val eval
    :  Environment.t
    -> 'model Incr.t
    -> inject:('action -> Event.t)
    -> ('model, 'action, 'result) Computation.t
    -> ('model, 'action, 'result) Snapshot.t Incr.t

  val reveal_value : 'a Proc.Value.t -> 'a Value.t
  val conceal_value : 'a Value.t -> 'a Proc.Value.t
  val reveal_computation : 'a Proc.Computation.t -> 'a Computation.packed
  val conceal_computation : 'a Computation.packed -> 'a Proc.Computation.t
end
