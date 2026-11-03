open! Core
open Bonsai.Private

module Entry : sig
  type t =
    { node_id : Instrumentation.Profiled_event.Node_id.t
    ; node_type : string
    ; here : Source_code_position.t
    ; start_time : Time_ns.t
    ; duration : Time_ns.Span.t
    }
  [@@deriving bin_io, sexp]
end

module Message : sig
  (** The debugger/profiler receives both performance entries as well as a description of
      the Bonsai graph. *)
  type t = Performance_measure of Entry.t [@@deriving bin_io, sexp]
end

module Worker_message : sig
  type t =
    | Uuid of Uuid.t
    | Message of Message.t
end

module Stable : sig
  module Entry : sig
    module V1 : sig
      type t =
        { label : [ `Bonsai of Node_path.t | `Other of string ]
        ; entry_type : string
        ; start_time : float
        ; duration : float
        }
      [@@deriving sexp, bin_io]
    end

    module V2 : sig
      type t = Entry.t =
        { node_id : Instrumentation.Profiled_event.Node_id.t
        ; node_type : string
        ; here : Source_code_position.t
        ; start_time : Time_ns.t
        ; duration : Time_ns.Span.t
        }
      [@@deriving sexp, bin_io]
    end
  end

  module Message : sig
    module V1 : sig
      type t =
        | Graph_info of Graph_info.Stable.V1.t
        | Performance_measure of Entry.V1.t
      [@@deriving sexp, bin_io]
    end

    module V2 : sig
      type t =
        | Graph_info of Graph_info.Stable.V2.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp]

      val of_v1 : V1.t -> t
    end

    module V3 : sig
      type t =
        | Graph_info of Graph_info.Stable.V3.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp]

      val of_v2 : V2.t -> t
    end

    module V4 : sig
      type t =
        | Graph_info of Graph_info.Stable.Node_info.V3.t Node_path.Stable.V1.Map.t
        | Performance_measure of Entry.V1.t
      [@@deriving bin_io, sexp]

      val of_v3 : V3.t -> t
    end

    module V5 : sig
      type t = Message.t = Performance_measure of Entry.V2.t [@@deriving bin_io, sexp]
    end
  end

  module Worker_message : sig
    module V1 : sig
      type t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V2.t
      [@@deriving bin_io, sexp]
    end

    module V2 : sig
      type t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V3.t
      [@@deriving sexp, bin_io]

      val of_v1 : V1.t -> t
    end

    module V3 : sig
      type t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V4.t
      [@@deriving sexp, bin_io]

      val of_v2 : V2.t -> t
    end

    module V4 : sig
      type t = Worker_message.t =
        | Uuid of Uuid.Stable.V1.t
        | Message of Message.V5.t
      [@@deriving sexp, bin_io]
    end
  end
end

module Versioned_message : sig
  type t =
    | V1 of Stable.Message.V1.t list
    | V2 of Stable.Message.V2.t list
    | V3 of Stable.Worker_message.V1.t list
    | V4 of Stable.Worker_message.V2.t list
    | V5 of Stable.Worker_message.V3.t list
    | V6 of Stable.Message.V5.t list
  [@@deriving sexp, bin_io]

  val to_latest : t -> Stable.Message.V5.t list
end
