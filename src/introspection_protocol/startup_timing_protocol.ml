open! Core

module Duration = struct
  type t = Int63.t [@@deriving sexp]
end

module Gc_events = struct
  type t = Ppx_module_timer_runtime.For_introspection.Gc_events.t =
    { minor_collections : int
    ; major_collections : int
    ; compactions : int
    }
  [@@deriving sexp]
end

module Timing_event = struct
  type t = Ppx_module_timer_runtime.For_introspection.Timing_event.t =
    { description : string
    ; runtime : Duration.t
    ; gc_events : Gc_events.t
    ; nested_timing_events : t list
    }
  [@@deriving sexp]

  module Stable = struct
    type event = t [@@deriving sexp]
    type t = V1 of event [@@deriving sexp]

    let of_latest event = V1 event
    let to_latest (V1 event) = event
  end
end
