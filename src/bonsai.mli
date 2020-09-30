open! Core_kernel
open! Import

module Private : sig
  module Computation = Computation
  module Environment = Environment
  module Meta = Meta
  module Snapshot = Snapshot
  module Value = Value
  module Path = Path

  val eval
    :  environment:Environment.t
    -> path:Path.t
    -> model:'model Incr.t
    -> inject:('action -> Event.t)
    -> ('model, 'action, 'result) Computation.t
    -> ('model, 'action, 'result) Snapshot.t Incr.t

  val reveal_value : 'a Proc.Value.t -> 'a Value.t
  val conceal_value : 'a Value.t -> 'a Proc.Value.t
  val reveal_computation : 'a Proc.Computation.t -> 'a Computation.packed
  val conceal_computation : 'a Computation.packed -> 'a Proc.Computation.t
  val to_dot : 'a Proc.Computation.t -> string
  val path : Path.t Proc.Computation.t
end

module type Model = Module_types.Model
module type Action = Module_types.Action
module type Enum = Module_types.Enum
module type Comparator = Module_types.Comparator

type ('k, 'cmp) comparator = ('k, 'cmp) Module_types.comparator

module Event = Event

(*_ The following line has the effect of hiding the Private module from the public
  API.  The values therein are exposed in {!Private} above. *)
include module type of Proc with module Private := Proc.Private
module Arrow : module type of Legacy_api
