module Stable = struct
  module Private = struct
    module Node_path = Node_path.Stable
    module Graph_info = Graph_info.Stable
  end
end

open! Core
open! Import

module type Model = Module_types.Model
module type Action = Module_types.Action
module type Enum = Module_types.Enum
module type Comparator = Module_types.Comparator

type ('k, 'cmp) comparator = ('k, 'cmp) Module_types.comparator

module Effect = Effect
module Time_source = Time_source

module Private = struct
  module Computation = Computation
  module Environment = Environment
  module Input = Input
  module Meta = Meta
  module Snapshot = Snapshot
  module Lifecycle = Lifecycle
  module Value = Value
  module Path = Path
  module Node_path = Node_path
  module Graph_info = Graph_info
  module Instrumentation = Instrumentation
  module Flatten_values = Flatten_values
  module Constant_fold = Constant_fold
  module Skeleton = Skeleton
  module Transform = Transform
  module Linter = Linter
  module Pre_process = Pre_process
  include Proc.Private

  let path = Proc.path
  let gather = Eval.gather
  let pre_process = Pre_process.pre_process
end

module Expert = struct
  let thunk = Proc.thunk
  let assoc_on = Proc.assoc_on
end

include (Proc : module type of Proc with module Private := Proc.Private)

module For_open = struct
  module Computation = Computation
  module Effect = Effect
  module Value = Value
end

module Debug = struct
  let to_dot ?pre_process c = To_dot.to_dot ?pre_process (Private.reveal_computation c)
  let instrument_computation = Instrumentation.instrument_computation
  let enable_incremental_annotations = Annotate_incr.enable
  let disable_incremental_annotations = Annotate_incr.disable

  open Let_syntax

  let on_change v ~f =
    (* Use [after_display] because the incremental node is always considered to be in use.*)
    Edge.after_display
      (let%map v = v in
       f v;
       Effect.Ignore)
  ;;

  let on_change_print_s v sexp_of = on_change v ~f:(fun a -> print_s (sexp_of a))
end

module Arrow_deprecated = struct
  include Legacy_api
end
