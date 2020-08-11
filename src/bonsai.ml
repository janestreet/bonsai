open! Core_kernel
open! Import

let () = Incr.State.(set_max_height_allowed t 1024)

module type Model = Module_types.Model
module type Action = Module_types.Action

module Event = Event

module Private = struct
  module Snapshot = Snapshot
  module Environment = Environment
  module Computation = Computation
  module Value = Value

  let eval = Eval.eval

  include Proc.Private
end

include (Proc : module type of Proc with module Private := Proc.Private)

module Arrow = struct
  include Legacy_api
  module Event = Event
end
