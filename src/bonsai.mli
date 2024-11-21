(** Bonsai documentation can be found in [proc_intf.ml].

    The Bonsai API is currently in an intermediate state. It is transitioning from the
    "old" [Proc] API to the "new" [Cont] API. Currently the [Proc] API is the default and
    is included when you use [include Bonsai/_web/_web]. Current Bonsai documentation can
    be found in [proc_intf.ml]. *)

open! Core
open! Import
module Private_computation := Computation
module Private_value := Value

module Cont : sig
  include
    module type of Cont
    with module For_proc2 := Cont.For_proc2
     and module Conv := Cont.Conv

  module Bonsai : sig
    include
      module type of Cont
      with module For_proc2 := Cont.For_proc2
       and module Conv := Cont.Conv
  end
end

module Private : sig
  val reveal_value : 'a Cont.t -> 'a Private_value.t
  val conceal_value : 'a Private_value.t -> 'a Cont.t

  val top_level_handle
    :  ?here:Stdlib.Lexing.position
    -> (Cont.graph -> 'a Cont.t)
    -> 'a Private_computation.t

  val handle
    :  ?here:Stdlib.Lexing.position
    -> f:(Cont.graph -> 'a Cont.t)
    -> Cont.graph
    -> 'a Private_computation.t

  val perform
    :  ?here:Stdlib.Lexing.position
    -> Cont.graph
    -> 'a Private_computation.t
    -> 'a Cont.t

  val path : ?here:Stdlib.Lexing.position -> Cont.graph -> Path.t Cont.t

  module Value = Private_value
  module Computation = Private_computation
  module Input = Input
  module Environment = Environment
  module Meta = Meta
  module Snapshot = Snapshot
  module Lifecycle = Lifecycle
  module Path = Path
  module Action = Action
  module Stabilization_tracker = Stabilization_tracker
  module Node_path = Node_path
  module Graph_info = Graph_info
  module Instrumentation = Instrumentation
  module Flatten_values = Flatten_values
  module Constant_fold = Constant_fold
  module Enable_computation_watcher = Enable_computation_watcher
  module Skeleton = Skeleton
  module Transform = Transform
  module Linter = Linter
  module Trampoline = Trampoline
  module Annotate_incr = Annotate_incr
  module Computation_watcher = Computation_watcher

  val gather
    :  recursive_scopes:Computation.Recursive_scopes.t
    -> time_source:Time_source.t
    -> 'result Computation.t
    -> ('result, unit) Computation.packed_info

  val pre_process : 'result Computation.t -> 'result Computation.t
  val set_perform_on_exception : (exn -> unit) -> unit
end

module Proc : sig
  include
    Proc_intf.S
    with module Private_computation := Private_computation
     and module Private_value := Private_value
     and type 'a Value.t = 'a Cont.t
     and type 'a Computation.t = Cont.graph -> 'a Cont.t
     and type 'a Computation_status.t = 'a Cont.Computation_status.t
     and type 'a Dynamic_scope.t = 'a Cont.Dynamic_scope.t
     and type 'a Effect_throttling.Poll_result.t = 'a Cont.Effect_throttling.Poll_result.t
     and type 'a Var.t = 'a Cont.Expert.Var.t

  module Private = Private

  module Bonsai : sig
    include
      Proc_intf.S
      with module Private_computation := Private_computation
       and module Private_value := Private_value
       and type 'a Value.t = 'a Cont.t
       and type 'a Computation.t = Cont.graph -> 'a Cont.t
       and type 'a Computation_status.t = 'a Cont.Computation_status.t
       and type 'a Dynamic_scope.t = 'a Cont.Dynamic_scope.t
       and type 'a Effect_throttling.Poll_result.t =
        'a Cont.Effect_throttling.Poll_result.t
       and type 'a Var.t = 'a Cont.Expert.Var.t

    module Private = Private
  end
end

module Arrow_deprecated : sig
  include
    Legacy_api_intf.S
    with type ('input, 'result) t = 'input Cont.t -> Cont.graph -> 'result Cont.t
end

module Stable : sig
  module Private : sig
    module Node_path = Node_path.Stable
    module Graph_info = Graph_info.Stable
  end
end

include module type of Cont with module Map := Cont.Map

module For_open : sig
  module Effect = Effect
end
