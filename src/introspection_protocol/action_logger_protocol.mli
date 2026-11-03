open! Core
module Node_id = Bonsai.Private.Instrumentation.Profiled_event.Node_id

module Entry : sig
  type t =
    { action_sexp : Sexp.t option
    ; action_kind : [ `Static | `Dynamic | `Wrap ]
    ; start_time : Time_ns.t
    ; duration : Time_ns.Span.t
    ; stabilize_duration : Time_ns.Span.t option
    ; here : Source_code_position.t
    }
end

module Stable : sig
  module Entry : sig
    module V1 : sig
      type t = Entry.t [@@deriving sexp]
    end
  end
end

module Versioned_message : sig
  type t = V1 of Stable.Entry.V1.t list [@@deriving bin_io, sexp]

  val to_latest : t -> Stable.Entry.V1.t list
end
