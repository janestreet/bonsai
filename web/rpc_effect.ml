open! Core
open Async_kernel
open Async_rpc_kernel

module Where_to_connect = struct
  module Custom = struct
    type t = ..
  end

  type t =
    | Self
    | Url of string
    | Custom of Custom.t
end

module Rvar : sig
  (** A "Refreshable" var. *)
  type 'a t

  (** Makes a new container that asynchronously computes its contents on demand. *)
  val create : (unit -> 'a Deferred.Or_error.t) -> 'a t

  (** Mark the current contents of the container as being no longer valid,
      which means that the next time anyone wants to look at it, it must be
      re-computed. *)
  val invalidate : 'a t -> unit

  (** Computes the container's contents in order to return them. Getting the
      contents of the same ['a t] twice should only force computation once,
      unless it is invalidated between the calls to [contents], or the first
      call returns an error before the second call begins.

      If [invalidate] is called in the middle of computing the result, the
      computation starts over. *)
  val contents : 'a t -> 'a Deferred.Or_error.t
end = struct
  type 'a state =
    | Invalid
    | Pending
    | Value of 'a

  type 'a t =
    { mutable state : 'a state
    ; f : unit -> 'a Deferred.Or_error.t
    ; finished : ('a Or_error.t, read_write) Bvar.t
    }

  let create f = { state = Invalid; f; finished = Bvar.create () }
  let invalidate t = t.state <- Invalid

  let return_result t result =
    Deferred.return
      (match result with
       | Ok value ->
         t.state <- Value value;
         Bvar.broadcast t.finished (Ok value);
         Ok value
       | Error e ->
         t.state <- Invalid;
         Bvar.broadcast t.finished (Error e);
         Error e)
  ;;

  let rec contents t =
    match t.state with
    | Invalid ->
      t.state <- Pending;
      (match%bind Monitor.try_with_join_or_error t.f with
       | Ok value ->
         (match t.state with
          | Invalid ->
            (* If [t] has been invalidated in the middle of computing its
               result, try again. This recursive call shouldn't cause an infinite
               loop because [t.f] is passed when the [t] is created, which
               means it cannot possibly unconditionally call [invalidate]
               on itself. Undoubtedly there is a way around this that will cause
               an infinite loop, but in that case the infinite loop is not
               surprising. *)
            contents t
          | Pending -> return_result t (Ok value)
          | Value value ->
            eprint_s
              [%message
                "BUG: Skipped computing Rvar result because it has already been computed."];
            return_result t (Ok value))
       | Error e -> return_result t (Error e))
    | Pending -> Bvar.wait t.finished
    | Value value -> Deferred.Or_error.return value
  ;;
end

module Connector = struct
  type t =
    | Async_durable :
        { connection : Rpc.Connection.t Async_durable.t
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
        -> t
    | Persistent_connection :
        { connection_module :
            (module Persistent_connection.S
               with type t = 'conn
                and type conn = Rpc.Connection.t)
        ; connection : 'conn
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
        -> t
    | Connection :
        { connection : Rpc.Connection.t Deferred.t
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
        -> t
    | Test_fallback : t

  let persistent_connection
    (type conn)
    (module Conn : Persistent_connection.S
      with type t = conn
       and type conn = Rpc.Connection.t)
    (connection : conn)
    =
    let menu =
      Rvar.create (fun () ->
        let%bind connection = Conn.connected connection in
        Versioned_rpc.Menu.request connection)
    in
    Bus.iter_exn (Conn.event_bus connection) [%here] ~f:(function
      | Disconnected -> Rvar.invalidate menu
      | _ -> ());
    Persistent_connection { connection_module = (module Conn); connection; menu }
  ;;

  let async_durable (connection : Rpc.Connection.t Async_durable.t) =
    let menu =
      Rvar.create (fun () -> Async_durable.with_ connection ~f:Versioned_rpc.Menu.request)
    in
    Bus.iter_exn (Async_durable.is_intact_bus connection) [%here] ~f:(fun is_intact ->
      if not is_intact then Rvar.invalidate menu);
    Async_durable { connection; menu }
  ;;

  let for_test implementations ~connection_state =
    let open Async_rpc_kernel in
    let open Async_kernel in
    let to_server = Pipe.create () in
    let to_client = Pipe.create () in
    let one_connection implementations ~connection_state pipe_to pipe_from =
      let transport =
        Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
      in
      let%bind conn =
        Rpc.Connection.create ?implementations ~connection_state transport
      in
      return (Result.ok_exn conn)
    in
    don't_wait_for
      (let%bind server_conn =
         one_connection (Some implementations) ~connection_state to_server to_client
       in
       Rpc.Connection.close_finished server_conn);
    let connection =
      one_connection None ~connection_state:(fun _conn -> ()) to_client to_server
    in
    Connection
      { connection
      ; menu =
          Rvar.create (fun () ->
            let%bind connection = connection in
            Versioned_rpc.Menu.request connection)
      }
  ;;

  let test_fallback = Test_fallback

  let with_connection f ~where_to_connect ~callback =
    match f where_to_connect with
    | Async_durable { connection; menu = _ } -> Async_durable.with_ connection ~f:callback
    | Persistent_connection { connection_module = (module Conn); connection; menu = _ } ->
      let%bind connection = Conn.connected connection in
      callback connection
    | Connection { connection; menu = _ } ->
      let%bind connection = connection in
      callback connection
    | Test_fallback ->
      Deferred.Or_error.error_string
        "RPC not handled because no connector has been provided."
    | exception e -> Deferred.Or_error.of_exn e
  ;;

  let with_connection_with_menu f ~where_to_connect ~callback =
    match f where_to_connect with
    | Async_durable { connection; menu } ->
      Async_durable.with_ connection ~f:(fun connection ->
        let%bind.Deferred.Or_error menu = Rvar.contents menu in
        callback (Versioned_rpc.Connection_with_menu.create_directly connection menu))
    | Persistent_connection { connection_module = (module Conn); connection; menu } ->
      let%bind connection = Conn.connected connection in
      let%bind.Deferred.Or_error menu = Rvar.contents menu in
      callback (Versioned_rpc.Connection_with_menu.create_directly connection menu)
    | Connection { connection; menu } ->
      let%bind connection = connection in
      let%bind.Deferred.Or_error menu = Rvar.contents menu in
      callback (Versioned_rpc.Connection_with_menu.create_directly connection menu)
    | Test_fallback ->
      Deferred.Or_error.error_string
        "RPC not handled because no connector has been provided."
    | exception e -> Deferred.Or_error.of_exn e
  ;;
end

let connector_var =
  Bonsai.Dynamic_scope.create
    ~name:"Bonsai_web.Rpc_effect.connector_var"
    ~fallback:(fun _ -> failwith "BUG: no bonsai-rpc handler installed")
    ()
;;

module Private = struct
  let with_connector connector computation =
    Bonsai.Dynamic_scope.set
      connector_var
      (Bonsai.Value.return connector)
      ~inside:computation
  ;;

  let self_connector =
    lazy
      (Connector.persistent_connection
         (module Persistent_connection.Rpc)
         (Persistent_connection.Rpc.create
            ~server_name:"self-ws-server"
            ~address:(module Unit)
            ~connect:(fun () -> Async_js.Rpc.Connection.client ())
            Deferred.Or_error.return))
  ;;

  let self_connector () = Lazy.force self_connector

  let url_connector =
    Memo.of_comparable
      (module String)
      (fun url ->
        Connector.persistent_connection
          (module Persistent_connection.Rpc)
          (Persistent_connection.Rpc.create
             ~server_name:"self-ws-server"
             ~address:(module String)
             ~connect:(fun url ->
               Async_js.Rpc.Connection.client ~uri:(Uri.of_string url) ())
             (fun () -> Deferred.Or_error.return url)))
  ;;

  let is_test_fallback connector =
    match connector with
    | Connector.Test_fallback -> true
    | Async_durable _ | Persistent_connection _ | Connection _ -> false
  ;;

  module For_tests = struct
    module Rvar = Rvar
  end
end

module Poll_result = struct
  type ('query, 'response) t =
    { last_ok_response : ('query * 'response) option
    ; last_error : ('query * Error.t) option
    ; inflight_query : 'query option
    ; refresh : (unit Effect.t[@sexp.opaque])
    }
  [@@deriving sexp_of]
end

module Shared_poller = struct
  open Bonsai.Let_syntax

  type ('query, 'response) t = ('query, ('query, 'response) Poll_result.t) Bonsai.Memo.t

  let create = Bonsai.Memo.create
  let custom_create = create

  let lookup ?sexp_of_model ~equal memo query =
    let%sub res = Bonsai.Memo.lookup ?sexp_of_model ~equal memo query in
    match%arr res with
    | Some x -> x
    | None ->
      { Poll_result.last_ok_response = None
      ; last_error = None
      ; inflight_query = None
      ; refresh = Effect.Ignore
      }
  ;;
end

module Inflight_query_key = Unique_id.Int ()

module Poll_behavior = struct
  type t =
    | Always (* Sends an rpc on every clock tick. *)
    | Until_ok
  (* Sends an rpc repeatedly until an ok response arrives. Stops polling
       once an error occurs.*)
end

let generic_poll_or_error
  (type query response)
  ~sexp_of_query
  ~sexp_of_response
  ~equal_query
  ?(equal_response = phys_equal)
  ~clear_when_deactivated
  ~on_response_received
  dispatcher
  ~every
  ~poll_behavior
  query
  =
  let module Query = struct
    type t = query

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_query
  end
  in
  let module Response = struct
    type t = response

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_response
  end
  in
  let open Bonsai.Let_syntax in
  let module Model = struct
    let sexp_of_query = Query.sexp_of_t
    let sexp_of_response = Response.sexp_of_t

    type t =
      { last_ok_response : (query * response) option
      ; last_error : (query * Error.t) option
      ; inflight_queries : query Inflight_query_key.Map.t
      }
    [@@deriving sexp_of, equal]
  end
  in
  let module Action = struct
    type t =
      | Finish of
          { query : Query.t
          ; response : Response.t Or_error.t Bonsai.Effect_throttling.Poll_result.t
          ; inflight_query_key : Inflight_query_key.t
          }
      | Start of
          { query : Query.t
          ; inflight_query_key : Inflight_query_key.t
          }
    [@@deriving sexp_of]
  end
  in
  let default_model =
    { Model.last_ok_response = None
    ; last_error = None
    ; inflight_queries = Inflight_query_key.Map.empty
    }
  in
  let%sub response, inject_response =
    (* using a state_machine1 is important because we need add check the Computation_status
       to see if we should drop the action (due to [clear_when_responded]) *)
    Bonsai.state_machine1
      (* Use a var here to prevent bonsai from optimizing the [state_machine1] down to a
         [state_machine0] *)
      Bonsai.Var.(create () |> value)
      ~sexp_of_model:[%sexp_of: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal:[%equal: Model.t]
      ~default_model
      ~apply_action:
        (fun
          (_ : _ Bonsai.Apply_action_context.t) computation_status model action ->
        let should_ignore =
          match computation_status with
          | Inactive -> clear_when_deactivated
          | Active () -> false
        in
        if should_ignore
        then default_model
        else (
          match action with
          | Finish { query; response; inflight_query_key } ->
            let last_ok_response, last_error =
              match response with
              | Finished (Ok response) -> Some (query, response), None
              | Finished (Error error) -> model.last_ok_response, Some (query, error)
              | Aborted -> model.last_ok_response, model.last_error
            in
            { last_ok_response
            ; last_error
            ; inflight_queries = Map.remove model.inflight_queries inflight_query_key
            }
          | Start { query; inflight_query_key } ->
            { model with
              inflight_queries =
                Map.add_exn model.inflight_queries ~key:inflight_query_key ~data:query
            }))
  in
  let%sub effect =
    let%arr dispatcher = dispatcher
    and inject_response = inject_response
    and on_response_received = on_response_received in
    fun query ->
      let open Effect.Let_syntax in
      let%bind inflight_query_key = Effect.of_sync_fun Inflight_query_key.create () in
      let%bind () = inject_response (Start { query; inflight_query_key }) in
      let%bind response = dispatcher query in
      let%bind () = inject_response (Finish { query; response; inflight_query_key }) in
      match response with
      | Bonsai.Effect_throttling.Poll_result.Aborted -> Effect.Ignore
      | Bonsai.Effect_throttling.Poll_result.Finished response ->
        on_response_received query response
  in
  (* Below are three constructs that schedule the effect to run. The tricky part
     of this is that [Clock.every] and [Edge.on_change] both run effects on
     activate by default. To avoid the redundancy, we make neither of them
     trigger on activate, and only use [on_activate] for running effects on
     activation. *)
  let%sub callback =
    let%arr effect = effect in
    fun prev query ->
      match prev with
      | Some _ -> effect query
      | None -> Effect.Ignore
  in
  let%sub () =
    Bonsai.Edge.on_change'
      ~sexp_of_model:[%sexp_of: Query.t]
      ~equal:equal_query
      query
      ~callback
  in
  let%sub send_rpc_effect =
    let%arr effect = effect
    and query = query in
    effect query
  in
  let%sub () =
    let clock =
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
        ~trigger_on_activate:false
        every
        send_rpc_effect
    in
    match poll_behavior with
    | Poll_behavior.Always -> clock
    | Until_ok ->
      let%sub should_poll =
        let%arr { last_ok_response; last_error; _ } = response in
        Option.is_none last_ok_response || Option.is_some last_error
      in
      (match%sub should_poll with
       | true -> clock
       | false -> Bonsai.const ())
  in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate:send_rpc_effect () in
  let%arr { last_ok_response; last_error; inflight_queries } = response
  and send_rpc_effect = send_rpc_effect in
  let inflight_query = Option.map ~f:snd (Map.max_elt inflight_queries) in
  { Poll_result.last_ok_response; last_error; inflight_query; refresh = send_rpc_effect }
;;

(* This [generic_poll_or_error] refines the [generic_poll_or_error] above by
   resetting on deactivate to avoid leaking memory (after all, an important
   feature of [Polling_state_rpc.dispatcher] is that doesn't cause a memory
   leak on the server, so it would be shame if we didn't also defend against
   memory leaks on the client. *)
let generic_poll_or_error
  ~sexp_of_query
  ~sexp_of_response
  ~equal_query
  ?equal_response
  ?(clear_when_deactivated = true)
  ?(on_response_received = Bonsai.Value.return (fun _ _ -> Effect.Ignore))
  dispatcher
  ~every
  ~poll_behavior
  query
  =
  let c =
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ~on_response_received
      ~clear_when_deactivated
      dispatcher
      ~every
      ~poll_behavior
      query
  in
  let open Bonsai.Let_syntax in
  if clear_when_deactivated
  then (
    let%sub result, reset = Bonsai.with_model_resetter c in
    let%sub () = Bonsai.Edge.lifecycle ~on_deactivate:reset () in
    return result)
  else c
;;

module Our_rpc = struct
  let generic_dispatcher (type request response) dispatcher
    : (request -> response Effect.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub connector = Bonsai.Dynamic_scope.lookup connector_var in
    let%arr connector = connector in
    Effect.of_deferred_fun (dispatcher connector)
  ;;

  let dispatcher rpc ~where_to_connect =
    generic_dispatcher (fun connector query ->
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        Rpc.Rpc.dispatch rpc connection query))
  ;;

  let babel_dispatcher rpc ~where_to_connect =
    generic_dispatcher (fun connector query ->
      Connector.with_connection_with_menu
        connector
        ~where_to_connect
        ~callback:(fun connection -> Babel.Caller.Rpc.dispatch_multi rpc connection query))
  ;;

  let streamable_dispatcher rpc ~where_to_connect =
    generic_dispatcher (fun connector query ->
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        Streamable.Plain_rpc.dispatch rpc connection query))
  ;;

  let poll
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = dispatcher rpc ~where_to_connect in
    let%sub dispatcher = Bonsai.Effect_throttling.poll dispatcher in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every
      ~poll_behavior:Always
      query
  ;;

  let babel_poll
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = babel_dispatcher rpc ~where_to_connect in
    let%sub dispatcher = Bonsai.Effect_throttling.poll dispatcher in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every
      ~poll_behavior:Always
      query
  ;;

  let streamable_poll
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = streamable_dispatcher rpc ~where_to_connect in
    let%sub dispatcher = Bonsai.Effect_throttling.poll dispatcher in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every
      ~poll_behavior:Always
      query
  ;;

  let shared_poller
    (type q cmp)
    (module Q : Bonsai.Comparator with type t = q and type comparator_witness = cmp)
    ?sexp_of_response
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    =
    let module M = struct
      include Q

      let equal a b = Q.comparator.compare a b = 0
    end
    in
    Shared_poller.create
      (module Q)
      ~f:(fun query ->
        poll
          ~sexp_of_query:M.sexp_of_t
          ?sexp_of_response
          ~equal_query:M.equal
          ?equal_response
          ?clear_when_deactivated
          ?on_response_received
          rpc
          ~where_to_connect
          ~every
          query)
  ;;

  let poll_until_ok
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~retry_interval
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = dispatcher rpc ~where_to_connect in
    let%sub dispatcher = Bonsai.Effect_throttling.poll dispatcher in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every:retry_interval
      ~poll_behavior:Until_ok
      query
  ;;

  let babel_poll_until_ok
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~retry_interval
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = babel_dispatcher rpc ~where_to_connect in
    let%sub dispatcher = Bonsai.Effect_throttling.poll dispatcher in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every:retry_interval
      ~poll_behavior:Until_ok
      query
  ;;
end

module Polling_state_rpc = struct
  let dispatcher ?(on_forget_client_error = fun _ -> Effect.Ignore) rpc ~where_to_connect =
    let open Bonsai.Let_syntax in
    let%sub connector = Bonsai.Dynamic_scope.lookup connector_var in
    let%sub client =
      Bonsai.Expert.thunk (fun () -> Polling_state_rpc.Client.create rpc)
    in
    let%sub forget_client_on_server =
      let perform_dispatch (connector, client) =
        Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
          match%map.Deferred
            Polling_state_rpc.Client.forget_on_server client connection
          with
          | Ok () -> Ok ()
          | Error _ when Rpc.Connection.is_closed connection ->
            (* If the connection is closed, then any data for this
               connection has been forgotten by the server anyway, so
               the error is moot. *)
            Ok ()
          | Error error -> Error error)
      in
      let%arr connector = connector
      and client = client in
      match%bind.Effect Effect.of_deferred_fun perform_dispatch (connector, client) with
      | Ok () -> Effect.Ignore
      | Error error -> on_forget_client_error error
    in
    let%sub () = Bonsai.Edge.lifecycle ~on_deactivate:forget_client_on_server () in
    let perform_query (connector, client) query =
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        Polling_state_rpc.Client.dispatch client connection query)
    in
    let%arr connector = connector
    and client = client in
    Effect.of_deferred_fun (perform_query (connector, client))
  ;;

  let poll
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    query
    =
    let open Bonsai.Let_syntax in
    let%sub dispatcher = dispatcher rpc ~where_to_connect in
    let%sub dispatcher =
      let%arr dispatcher = dispatcher in
      fun query ->
        let%map.Effect result = dispatcher query in
        Bonsai.Effect_throttling.Poll_result.Finished result
    in
    generic_poll_or_error
      ~sexp_of_query
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~every
      ~poll_behavior:Always
      query
  ;;

  let shared_poller
    (type q cmp)
    (module Q : Bonsai.Comparator with type t = q and type comparator_witness = cmp)
    ?sexp_of_response
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    =
    let module M = struct
      include Q

      let equal a b = Q.comparator.compare a b = 0
    end
    in
    Shared_poller.create
      (module Q)
      ~f:(fun query ->
        poll
          ~sexp_of_query:M.sexp_of_t
          ?sexp_of_response
          ~equal_query:[%equal: M.t]
          ?equal_response
          ?clear_when_deactivated
          ?on_response_received
          rpc
          ~where_to_connect
          ~every
          query)
  ;;
end

module Status = struct
  open Bonsai.Let_syntax

  module State = struct
    type t =
      | Connecting
      | Connected
      | Disconnected of Error.t
      | Failed_to_connect of Error.t
    [@@deriving sexp, equal]
  end

  (* This is a weird "dispatcher" component because it doesn't try to send an RPC
     at all; it only tries to make the connection, making not of all the events
     that occurred in the process. *)
  let dispatcher ~where_to_connect =
    Our_rpc.generic_dispatcher (fun connector (writeback : State.t -> unit) ->
      match%map.Deferred
        Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
          writeback Connected;
          upon (Rpc.Connection.close_reason connection ~on_close:`started) (fun reason ->
            writeback (Disconnected (Error.of_info reason)));
          Deferred.Or_error.return ())
      with
      | Ok () -> ()
      | Error error ->
        (* We know that an error indicates a failure to connect because
           [callback] never returns an error of its own. *)
        writeback (Failed_to_connect error))
  ;;

  module Model = struct
    type state =
      | Initial
      | State of State.t
    [@@deriving sexp, equal]

    type nonrec t =
      { state : state
      ; clock : (Bonsai.Time_source.t option[@sexp.opaque] [@equal.ignore])
      ; connecting_since : Time_ns.Alternate_sexp.t option
      }
    [@@deriving sexp, equal]
  end

  module Action = struct
    type nonrec t =
      | Set of State.t
      | Activate of (Bonsai.Time_source.t[@sexp.opaque])
    [@@deriving sexp_of]
  end

  module Result = struct
    type t =
      { state : State.t
      ; connecting_since : Time_ns.Alternate_sexp.t option
      }
    [@@deriving sexp_of]
  end

  let state ~where_to_connect =
    let%sub dispatcher = dispatcher ~where_to_connect in
    let%sub model, inject =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
        ~sexp_of_action:[%sexp_of: Action.t]
        dispatcher
        ~default_model:{ state = Initial; clock = None; connecting_since = None }
        ~apply_action:(fun context dispatcher model action ->
        let writeback a =
          Bonsai.Apply_action_context.schedule_event
            context
            (Bonsai.Apply_action_context.inject context (Set a))
        in
        let state = model.state in
        let new_state =
          match action, dispatcher with
          | Activate _, Inactive ->
            (* The activate message got to us, but we became inactive in the interim *)
            state
          | Activate _, Active dispatch ->
            (match state with
             | Initial | State (Disconnected _ | Failed_to_connect _) ->
               Bonsai.Apply_action_context.schedule_event context (dispatch writeback);
               State Connecting
             | State (Connecting | Connected) ->
               (* We got activated, but we're still listening to the previous connection. *)
               state)
          | Set new_state, Active dispatch ->
            (match new_state with
             | Failed_to_connect _ | Disconnected _ ->
               (* we failed, but we're still active, so try to reconnect *)
               Bonsai.Apply_action_context.schedule_event context (dispatch writeback)
             | Connected | Connecting -> ());
            State new_state
          | Set new_state, Inactive -> State new_state
        in
        let clock =
          match action with
          | Activate clock -> Some clock
          | Set _ -> model.clock
        in
        let connecting_since =
          let now () = Option.map ~f:Bonsai.Time_source.now clock in
          match state with
          | State Connected ->
            (match new_state with
             | State Connected -> model.connecting_since
             | Initial | State (Connecting | Disconnected _ | Failed_to_connect _) ->
               now ())
          | Initial -> now ()
          | State _ -> model.connecting_since
        in
        { state = new_state; clock; connecting_since })
    in
    let%sub () =
      let%sub clock = Bonsai.Incr.with_clock Ui_incr.return in
      let%sub on_activate =
        let%arr inject = inject
        and clock = clock in
        inject (Activate clock)
      in
      Bonsai.Edge.lifecycle ~on_activate ()
    in
    let%arr { Model.state; connecting_since; _ } = model in
    let state =
      match state with
      | State status -> status
      | Initial -> Connecting
    in
    let connecting_since =
      match state with
      | Connected -> None
      | Connecting | Disconnected _ | Failed_to_connect _ -> connecting_since
    in
    { Result.state; connecting_since }
  ;;

  include Result
end

module Rpc = Our_rpc
