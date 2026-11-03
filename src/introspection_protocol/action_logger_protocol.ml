open! Core
module Node_id = Bonsai.Private.Instrumentation.Profiled_event.Node_id

module Stable = struct
  open Core.Core_stable

  module Entry = struct
    module V1 = struct
      type t =
        { action_sexp : Sexp.V1.t option
        ; action_kind : [ `Static | `Dynamic | `Wrap ]
        ; start_time : Time_ns.V1.t
        ; duration : Time_ns.Span.V2.t
        ; stabilize_duration : Time_ns.Span.V2.t option
        ; here : Source_code_position.V1.t
        }
      [@@deriving bin_io, sexp]
    end
  end
end

module Entry = struct
  type t = Stable.Entry.V1.t =
    { action_sexp : Sexp.t option
    ; action_kind : [ `Static | `Dynamic | `Wrap ]
    ; start_time : Time_ns.t
    ; duration : Time_ns.Span.t
    ; stabilize_duration : Time_ns.Span.t option
    ; here : Source_code_position.t
    }
end

module Versioned_message = struct
  type t = V1 of Stable.Entry.V1.t list [@@deriving bin_io, sexp]

  let to_latest = function
    | V1 v1 -> v1
  ;;
end
