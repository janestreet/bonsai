open! Core

module type Devtool_page = sig
  val supported_version : float
  val is_introspection_supported_var_name : string
end

module type Recordable = sig
  include Devtool_page

  val is_recording_var_name : string
  val pop_events_var_name : string

  module Is_recording : sig
    type t [@@deriving string]

    val of_bool : bool -> t
  end
end

module Bool_recording = struct
  include Bool

  let of_bool = Fn.id
end

module Profiling_recording = struct
  include Bonsai.Private.Instrumentation.Profiling

  let to_string t = t |> sexp_of_t |> Sexp.to_string
  let of_string t = t |> Sexp.of_string |> t_of_sexp

  let of_bool = function
    | true -> Profiling
    | false -> Not_profiling
  ;;
end

module Rpc_effect = struct
  let supported_version = 2.
  let is_introspection_supported_var_name = "rpcEffectIntrospectionSupported"
  let is_recording_var_name = "rpcEffectIsRecording"
  let pop_events_var_name = "rpcEffectPopEvents"

  module Is_recording = Bool_recording
end

module Bonsai_bug = struct
  let supported_version = 3.
  let is_introspection_supported_var_name = "bonsaiBugIntrospectionSupported"
  let is_recording_var_name = "bonsaiBugIsProfiling"
  let pop_events_var_name = "bonsaiBugPopEvents"
  let start_computation_watcher_var_name = "bonsaiBugStartComputationWatcher"
  let stop_computation_watcher_var_name = "bonsaiBugStopComputationWatcher"

  module Is_recording = Profiling_recording
end

module Incr_node_counts = struct
  let supported_version = 2.
  let is_introspection_supported_var_name = "bonsaiIncrNodeIntrospectionSupported"
  let is_recording_var_name = "incrNodeIsRecording"
  let pop_events_var_name = "bonsaiIncrNodeIntrospectionPopEvents"

  module Is_recording = Bool_recording
end

module Ppx_module_timer = struct
  let supported_version = 1.
  let is_introspection_supported_var_name = "ppxModuleTimerIntrospectionSupported"
  let read_module_timing_events_var_name = "ppxModuleTimerReadEvents"
end

module Action_logger = struct
  let supported_version = 3.
  let is_introspection_supported_var_name = "bonsaiActionLoggerIntrospectionSupported"
  let is_recording_var_name = "actionLoggerIsProfiling"
  let pop_events_var_name = "bonsaiActionLoggerPopEvents"

  module Is_recording = Profiling_recording
end

module Runtime = struct
  let revision_var_name = "bonsaiWebRevision"
  let is_bonsai_application_var_name = "isBonsaiApplication"
  let has_started_var_name = "bonsaiHasStarted"
end
