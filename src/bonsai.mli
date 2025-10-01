(** Bonsai documentation can be found in [cont.mli].

    The Bonsai API is currently in an intermediate state. It is transitioning from the
    "old" [Proc] API to the "new" [Cont] API. The [Cont] API is now the default and is
    included when you use [include Bonsai/_web/_web], but [Proc] style code still exists
    in some places. Current Bonsai documentation can be found in [cont.mli]. *)

open! Core
open! Import
module Private_computation := Computation
module Private_value := Value
module For_proc := Cont.For_proc

module Cont : sig
  include
    module type of Cont with module For_proc := Cont.For_proc and module Conv := Cont.Conv

  module Bonsai : sig
    include
      module type of Cont
      with module For_proc := Cont.For_proc
       and module Conv := Cont.Conv
  end
end

module Private : sig
  val reveal_value : 'a Cont.t -> 'a Private_value.t
  val conceal_value : 'a Private_value.t -> 'a Cont.t

  val top_level_handle
    :  here:[%call_pos]
    -> (local_ Cont.graph -> 'a Cont.t)
    -> 'a Private_computation.t

  val handle
    :  here:[%call_pos]
    -> f:(local_ Cont.graph -> 'a Cont.t)
    -> local_ Cont.graph
    -> 'a Private_computation.t

  val perform
    :  here:[%call_pos]
    -> local_ Cont.graph
    -> 'a Private_computation.t
    -> 'a Cont.t

  val read : here:[%call_pos] -> 'a Private_value.t -> 'a Private_computation.t
  val path : here:[%call_pos] -> local_ Cont.graph -> Path.t Cont.t

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
  module Constant_fold = Constant_fold
  module Enable_computation_watcher = Enable_computation_watcher
  module Skeleton = Skeleton
  module Transform = Transform
  module Linter = Linter
  module Timer = Timer
  module Trampoline = Trampoline
  module Annotate_incr = Annotate_incr
  module Computation_watcher = Computation_watcher
  module For_proc = For_proc

  val gather
    :  recursive_scopes:Computation.Recursive_scopes.t
    -> time_source:Time_source.t
    -> 'result Computation.t
    -> ('result, unit) Computation.packed_info

  val pre_process : 'result Computation.t -> 'result Computation.t
  val set_perform_on_exception : (exn -> unit) -> unit
end

module Stable : sig
  module Private : sig
    module Node_path = Node_path.Stable
    module Graph_info = Graph_info.Stable
  end
end

(*_ We don't want to shadow [Map] for people doing [open Bonsai]. *)
include module type of Cont with module Map := Cont.Map

module For_open : sig
  module Effect = Effect
end
