open! Core_kernel
open! Import

let () = Incr.State.(set_max_height_allowed t 1024)

module type Model = Module_types.Model
module type Action = Module_types.Action
module type Enum = Module_types.Enum
module type Comparator = Module_types.Comparator

type ('k, 'cmp) comparator = ('k, 'cmp) Module_types.comparator

module Event = Event

module Private = struct
  module Computation = Computation
  module Environment = Environment
  module Meta = Meta
  module Snapshot = Snapshot
  module Value = Value

  let eval = Eval.eval

  include Proc.Private

  let to_dot c = To_dot.to_dot (reveal_computation c)
end

include (Proc : module type of Proc with module Private := Proc.Private)

module Arrow = struct
  include Legacy_api
  module Event = Event
end
