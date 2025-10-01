open! Core

(** These types are serialization types for the timings recorded by ppx_module_timer. The
    types are copied over from the ppx_module_timer_runtime library, but also have sexp
    functions and a stable type. *)
module Duration : sig
  type t = Int63.t [@@deriving sexp_of]
end

module Gc_events : sig
  type t = Ppx_module_timer_runtime.For_introspection.Gc_events.t =
    { minor_collections : int
    ; major_collections : int
    ; compactions : int
    }
  [@@deriving sexp_of]
end

module Timing_event : sig
  type t = Ppx_module_timer_runtime.For_introspection.Timing_event.t =
    { description : string
    ; runtime : Duration.t
    ; gc_events : Gc_events.t
    ; nested_timing_events : t list
    }
  [@@deriving sexp_of]

  module Stable : sig
    type event := t
    type t [@@deriving sexp]

    val of_latest : event -> t
    val to_latest : t -> event
  end
end
