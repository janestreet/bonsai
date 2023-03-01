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

module Poll_result : sig
  (** The various rpc polling functions in this module return a [Poll_result.t],
      containing the current state and some historical state of the RPC.

      [last_ok_response] contains the most recent query/response pair that completed
      successfully, even if the RPC has returned errors since then.

      [last_error] contains the most recent query that produced an error, alongside
      the error that was returned.  Unlike [last_ok_response], this field is set to
      [None] as soon a response completes sucessfully.

      [inflight_query] is [Some] when an a query has been dispatched, but has not
      completed yet.

      [refresh] can be used to manually redispatch the rpc *)

  type ('query, 'response) t =
    { last_ok_response : ('query * 'response) option
    ; last_error : ('query * Error.t) option
    ; inflight_query : 'query option
    ; refresh : unit Effect.t
    }
  [@@deriving sexp_of]
end

module Shared_poller : sig
  (** A [Shared_poller] is a handle to a polling-style RPC whose RPCs
      can be shared between multiple components that might have an interest
      in polling values with the same types.

      To create a [Shared_poller], use either [Rpc_effect.Rpc.shared_poller] or
      [Rpc_effect.Polling_state_rpc.shared_poller].  With the value returned by
      those functions, you can call [Shared_poller.lookup] with a query value to
      get access to the results of the given RPC with the provided query. *)

  type ('query, 'response) t

  (** Uses a shared-poller to either start polling an RPC, or if another user of the same
      shared-poller is already polling with the same query, it'll immediately return the
      most recent value. *)
  val lookup
    :  (module Bonsai.Model with type t = 'query)
    -> ('query, 'response) t Bonsai.Value.t
    -> 'query Bonsai.Value.t
    -> ('query, 'response) Poll_result.t Bonsai.Computation.t

  (** You can use [custom_create] to build a shared-poller if the
      [Rpc_effect.Rpc.shared_poller] and [Rpc_effect.Polling_state_rpc.shared_poller]
      aren't sufficient. *)
  val custom_create
    :  ('query, _) Bonsai.comparator
    -> f:('query Value.t -> ('query, 'response) Poll_result.t Computation.t)
    -> ('query, 'response) t Computation.t
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

  (** A computation that periodically dispatches on an RPC and
      keeps track of the most recent response. *)
  val poll
    :  (module Bonsai.Model with type t = 'query)
    -> (module Bonsai.Model with type t = 'response)
    -> ?clear_when_deactivated:bool
    -> ('query, 'response) Rpc.Rpc.t
    -> where_to_connect:Where_to_connect.t
    -> every:Time_ns.Span.t
    -> 'query Value.t
    -> ('query, 'response) Poll_result.t Computation.t

  val shared_poller
    :  ('query, _) Bonsai.comparator
    -> (module Bonsai.Model with type t = 'response)
    -> ?clear_when_deactivated:bool
    -> ('query, 'response) Rpc.Rpc.t
    -> where_to_connect:Where_to_connect.t
    -> every:Time_ns.Span.t
    -> ('query, 'response) Shared_poller.t Computation.t

  (** Like [poll], but stops polling the same input query after an ok response.
      If the query changes, the computation will resume polling until it
      receives another ok response. If the computation receives an error
      response, it will retry sending the RPC after waiting [retry_interval]. *)
  val poll_until_ok
    :  (module Bonsai.Model with type t = 'query)
    -> (module Bonsai.Model with type t = 'response)
    -> ?clear_when_deactivated:bool
    -> ('query, 'response) Rpc.Rpc.t
    -> where_to_connect:Where_to_connect.t
    -> retry_interval:Time_ns.Span.t
    -> 'query Value.t
    -> ('query, 'response) Poll_result.t Computation.t
end

module Polling_state_rpc : sig
  (** An effect for dispatching on a particular Polling_state_rpc with a
      particular query. When the computation is deactivated, it asks the server
      to cleanup any cached data, so that there is no memory leak. If this
      cleanup fails, then [on_forget_client_error] is called with the error. *)
  val dispatcher
    :  ?on_forget_client_error:(Error.t -> unit Effect.t)
    -> ('query, 'response) Polling_state_rpc.t
    -> where_to_connect:Where_to_connect.t
    -> ('query -> 'response Or_error.t Effect.t) Computation.t

  (** A computation that periodically dispatches on a polling_state_rpc and
      keeps track of the most recent response. To explicitly re-send the RPC,
      schedule the [refresh] field of the result. It also keeps track of the current
      query that is in-flight.*)
  val poll
    :  (module Bonsai.Model with type t = 'query)
    -> (module Bonsai.Model with type t = 'response)
    -> ?clear_when_deactivated:bool
    -> ('query, 'response) Polling_state_rpc.t
    -> where_to_connect:Where_to_connect.t
    -> every:Time_ns.Span.t
    -> 'query Value.t
    -> ('query, 'response) Poll_result.t Computation.t

  val shared_poller
    :  ('query, _) Bonsai.comparator
    -> (module Bonsai.Model with type t = 'response)
    -> ?clear_when_deactivated:bool
    -> ('query, 'response) Polling_state_rpc.t
    -> where_to_connect:Where_to_connect.t
    -> every:Time_ns.Span.t
    -> ('query, 'response) Shared_poller.t Computation.t
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
    [@@deriving equal, sexp]
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
