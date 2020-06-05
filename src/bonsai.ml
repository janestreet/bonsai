open! Core_kernel
open! Import
include Legacy_api
module Proc = Proc

let () = Incr.State.(set_max_height_allowed t 1024)

module Private = struct
  module Snapshot = Snapshot
  module Environment = Environment
  module Computation = Computation
  module Value = Value

  let eval = Eval.eval

  include Proc.Private
end
