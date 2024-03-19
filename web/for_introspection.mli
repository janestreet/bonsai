open! Core
open Bonsai_introspection_protocol

(** This module contains functions and effects for "introspecting" rpc effect rpc's. 

    This is how the Rpc_effect module communicates rpc information to the
    bonsai chrome extension's devtool panel. This module is meant to be
    used by bonsai internals and not by bonsai apps directly. *)
val should_record_effect : bool Effect.t

(** [register] registers the callbacks necessary for introspection to work/sets up the
    communication necessary for the chrome dev tool panel. It does not actually start do
    any introspection itself. Introspection is enabled/controlled by the panel via the 
    communication mechanisms setup by [register]. *)
val run_top_level_side_effects : unit -> unit

(** [send_and_track_rpc_from_poller] sends the [send_rpc] rpc, but it also
    records its start/finish event messages and "enqueue"'s the messages so
    that they can be read by the devtool panel.

    This function is specialized for "polling" functions that return a 
    ('query, 'response) Poll_result.t . For "dispatch" equivalents refer
    to [send_and_track_rpc_from_dispatch]. *)
val send_and_track_rpc_from_poller
  :  rpc_kind:Rpc_kind.t
  -> get_current_time:Time_ns.t Effect.t
  -> sexp_of_query:('query -> Sexp.t) option
  -> sexp_of_response:('response -> Sexp.t) option
  -> path:string
  -> send_rpc:
       ('query -> 'response Or_error.t Bonsai.Effect_throttling.Poll_result.t Ui_effect.t)
  -> query:'query
  -> 'response Or_error.t Bonsai.Effect_throttling.Poll_result.t Ui_effect.t

(** [send_and_track_rpc_from_dispatch] is like [send_and_track_rpc_from_poller] but
    for one-off dispatch functions. (e.g. [Rpc_effect.Rpc.dispatcher]). *)
val send_and_track_rpc_from_dispatch
  :  rpc_kind:Rpc_kind.t
  -> get_current_time:Time_ns.t Effect.t
  -> sexp_of_query:('query -> Sexp.t) option
  -> sexp_of_response:('response -> Sexp.t) option
  -> path:string
  -> send_rpc:('query -> 'response Or_error.t Ui_effect.t)
  -> query:'query
  -> 'response Or_error.t Ui_effect.t

module For_testing : sig
  val get_introspection_supported : unit -> bool
  val get_is_recording : unit -> bool
  val pop_events : unit -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

  (** [pop_event'] is like [pop_events] but gives the actual events for tests
      where the thing that is desired to test is not protocol/serialization, but rather
      more logical things. *)
  val pop_events' : unit -> Event.t list

  val start_recording : unit -> unit
  val stop_recording : unit -> unit
end
