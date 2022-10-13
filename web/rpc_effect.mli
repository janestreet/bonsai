open! Core
open! Async_kernel
open Async_rpc_kernel
open Bonsai.For_open

(** The place that an RPC should be sent. *)
module Where_to_connect : sig
  module Custom : sig
    type t = ..
  end

  type t =
    | Self
    | Url of string
    | Custom of Custom.t
end

module Rpc : sig
  (** An effect for sending a particular RPC to a particular place. *)
  val dispatcher
    :  ('query, 'response) Rpc.Rpc.t
    -> where_to_connect:Where_to_connect.t
    -> ('query -> 'response Or_error.t Effect.t) Computation.t

  val babel_dispatcher
    :  ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> where_to_connect:Where_to_connect.t
    -> ('query -> 'response Or_error.t Effect.t) Computation.t
end

module Status : sig
  module State : sig
    (** The status of an RPC connection.

        state diagram: {v

      START
       |       .------------------.
       v       v                   \
      Connecting -> Connected <-> Disconnected
       |  ^          ^
       v  |          |
      Failed_to_connect

      v} *)
    type t =
      | Connecting
      | Connected
      | Disconnected of Error.t
      | Failed_to_connect of Error.t
    [@@deriving sexp_of]
  end

  type t =
    { state : State.t
    ; connecting_since : Time_ns.t option
    }
  [@@deriving sexp_of]

  (** A component whose output tracks the state of a connection to a host. *)
  val state : where_to_connect:Where_to_connect.t -> t Computation.t
end

module Connector : sig
  (** A connector specifies a way of creating a connection. This module is
      exposed to cover exceptional cases; ordinarily, you should prefer to use
      the [Self] and [Url] constructors of [Where_to_connect.t], which have a
      connector backing them that you don't need to explicitly provide.  *)

  module Rpc := Async_rpc_kernel.Rpc

  type t

  val persistent_connection
    :  (module Persistent_connection.S with type t = 'conn and type conn = Rpc.Connection.t)
    -> 'conn
    -> t

  val async_durable : Rpc.Connection.t Async_durable.t -> t

  val for_test
    :  's Rpc.Implementations.t
    -> connection_state:(Rpc.Connection.t -> 's)
    -> t

  val test_fallback : t
end

module Private : sig
  (** This module contains functions intended for use by Bonsai's internal
      startup code. Ordinarily, you shouldn't need to call any of them.

      More specifically, in tests, [with_connector] is called when a test
      handle is created, using an optional, user-provided function to select
      the connector. Similarly, when an app is actually being run, we take a
      function of type [Custom.t -> Connector.t] and default the [Self] and
      [Url] cases to [self_connector] and [url_connector] declared below.  *)

  (** Turns a computation into a new computation that has access to some sort of
      connection. This is the primitive and most powerful way of providing access
      to a connection. Since it has access to the [Where_to_connect.t], it can
      create different kinds of connections based on what is being connected to.  *)
  val with_connector
    :  (Where_to_connect.t -> Connector.t)
    -> 'a Computation.t
    -> 'a Computation.t

  (** The connector for the server hosting the web page. *)
  val self_connector : unit -> Connector.t

  (** The connector for an arbitrary URL. *)
  val url_connector : string -> Connector.t

  (** Determines whether the connector is the test fallback connector. This is
      used by the testing library to swap out the [test_fallback] connector with
      a different connector controlled by other parameters. *)
  val is_test_fallback : Connector.t -> bool

  module For_tests : sig
    module Rvar : sig
      type 'a t

      val create : (unit -> 'a Deferred.Or_error.t) -> 'a t
      val invalidate : 'a t -> unit
      val contents : 'a t -> 'a Deferred.Or_error.t
    end
  end
end
