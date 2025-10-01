open! Core

(** Represents an rpc_id. This bears no resemblance/equality with async_rpc_kernel's rpc
    ids and is instead a bonsai-specific id. *)
module Rpc_id : Unique_id.Id with type t = private Int63.t

module Rpc_kind : sig
  module Interval : sig
    type t =
      | Poll_until_ok of { retry_interval : Time_ns.Span.t }
      | Poll_until_condition_met of { every : Time_ns.Span.t }
      | Poll of { every : Time_ns.Span.t }
      | Dispatch
    [@@deriving sexp, equal]
  end

  module Polling_state_rpc_interval : sig
    type t =
      | Poll of { every : Time_ns.Span.t }
      | Dispatch
    [@@deriving sexp, equal]
  end

  (** [Rpc_kind.t]'s correspond to the different rpc kinds available in
      [Bonsai_web.Rpc_effect]. *)
  type t =
    | Normal of
        { name : string
        ; version : int
        ; interval : Interval.t
        }
    | Babel of
        { descriptions : Async_rpc_kernel.Rpc.Description.t Nonempty_list.t
        ; interval : Interval.t
        }
    | Streamable of
        { name : string
        ; version : int
        ; interval : Interval.t
        }
    | Polling_state_rpc of
        { name : string
        ; version : int
        ; interval : Polling_state_rpc_interval.t
        }
    | Babel_polling_state_rpc of
        { descriptions : Async_rpc_kernel.Rpc.Description.t Nonempty_list.t
        ; interval : Polling_state_rpc_interval.t
        }
  [@@deriving sexp, equal]
end

module Or_no_sexp_of_provided : sig
  type 'a t =
    | No_sexp_of_provided
    | Sexp_of_provided of 'a
  [@@deriving sexp, equal]
end

module Rpc_status : sig
  type t =
    | Running
    | Finished of
        { duration : Time_ns.Span.t
        ; response : Sexp.t Or_no_sexp_of_provided.t Or_error.t
        }
    | Aborted of { duration : Time_ns.Span.t }
  [@@deriving sexp, equal]
end

module Event : sig
  module V1 : sig
    type t =
      | Started of
          { id : Rpc_id.t
          ; rpc_kind : Rpc_kind.t
          ; start_time : Time_ns.Alternate_sexp.t
          ; query : Sexp.t Or_no_sexp_of_provided.t
          ; path : string
          ; here : Source_code_position.t option
          }
      | Finished of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          ; response : Sexp.t Or_no_sexp_of_provided.t Or_error.t
          }
      | Aborted of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          }
      | Response_size of
          { id : Rpc_id.t
          ; payload_bytes : int
          }
  end

  type t = V1.t

  module Stable : sig
    type event := t

    (** [Event.t] are the only types serialized from inspected bonsai applications to the
        bonsai chrome extension's devtool panel. Due to the inspected applications and the
        chrome extension potentially being out of date, events must be stable typed. *)
    type t [@@deriving sexp, quickcheck]

    val to_latest : t -> event
    val of_latest : event -> t
  end
end

module Rpc_state : sig
  type t =
    { rpc_kind : Rpc_kind.t
    ; start_time : Time_ns.Alternate_sexp.t
    ; query : Sexp.t Or_no_sexp_of_provided.t
    ; status : Rpc_status.t
    ; path : string
    ; response_size : int option
    ; here : Source_code_position.t option
    }
  [@@deriving sexp]
end

module State : sig
  type t = Rpc_state.t Rpc_id.Map.t [@@deriving sexp, equal]

  val empty : t
  val apply_event : t -> Event.t -> t
end

module For_testing : sig
  module Rpc_id : sig
    type t = Rpc_id.t

    val of_int : int -> t
    val to_string : t -> string
  end

  module Event : sig
    type t =
      | Started of
          { id : Rpc_id.t
          ; rpc_kind : Rpc_kind.t
          ; start_time : Time_ns.Alternate_sexp.t
          ; query : Sexp.t Or_no_sexp_of_provided.t
          ; path : string
          ; here : Source_code_position.t option
          }
      | Finished of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          ; response : Sexp.t Or_no_sexp_of_provided.t Or_error.t
          }
      | Aborted of
          { id : Rpc_id.t
          ; duration : Time_ns.Span.t
          }
      | Response_size of
          { id : Rpc_id.t
          ; payload_bytes : int
          }

    module Unstable : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    val conceal : t -> Event.t
    val reveal : Event.t -> t
  end

  val reset_ids_for_testing : unit -> unit
end
