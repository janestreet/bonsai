open! Core

(** The names of the globals and session-storage variables through which a Bonsai
    application exposes introspection hooks to devtools. *)

module type Devtool_page = sig
  val supported_version : float
  val is_introspection_supported_var_name : string
end

module type Recordable = sig
  include Devtool_page

  val is_recording_var_name : string
  val pop_events_var_name : string

  (** Historically, different recording variables have been serialized/represented
      differently, so we need to specify how to convert & serialize each one. *)
  module Is_recording : sig
    type t [@@deriving string]

    val of_bool : bool -> t
  end
end

module Rpc_effect : Recordable with type Is_recording.t = bool

module Bonsai_bug : sig
  include Recordable with type Is_recording.t = Bonsai.Private.Instrumentation.Profiling.t

  val start_computation_watcher_var_name : string
  val stop_computation_watcher_var_name : string
end

module Incr_node_counts : Recordable with type Is_recording.t = bool

module Ppx_module_timer : sig
  include Devtool_page

  val read_module_timing_events_var_name : string
end

module Action_logger :
  Recordable with type Is_recording.t = Bonsai.Private.Instrumentation.Profiling.t

module Runtime : sig
  val revision_var_name : string
  val is_bonsai_application_var_name : string
  val has_started_var_name : string
end
