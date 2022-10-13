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
       | Error e ->
         return_result t (Error e))
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
    let one_connection implementations pipe_to pipe_from =
      let transport =
        Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
      in
      let%bind conn =
        Rpc.Connection.create ?implementations ~connection_state transport
      in
      return (Result.ok_exn conn)
    in
    don't_wait_for
      (let%bind server_conn = one_connection (Some implementations) to_server to_client in
       Rpc.Connection.close_finished server_conn);
    let connection = one_connection None to_client to_server in
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
        ~callback:(fun connection ->
          Babel.Caller.Rpc.dispatch_multi rpc connection query))
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
        Connector.with_connection
          connector
          ~where_to_connect
          ~callback:(fun connection ->
            writeback Connected;
            upon
              (Rpc.Connection.close_reason connection ~on_close:`started)
              (fun reason -> writeback (Disconnected (Error.of_info reason)));
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
      ; clock : (Ui_incr.Clock.t option[@sexp.opaque] [@equal.ignore])
      ; connecting_since : Time_ns.Alternate_sexp.t option
      }
    [@@deriving sexp, equal]
  end

  module Action = struct
    type nonrec t =
      | Set of State.t
      | Activate of Ui_incr.Clock.t
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
      Bonsai.Expert.race
        (module Model)
        (module Action)
        dispatcher
        ~default_model:{ state = Initial; clock = None; connecting_since = None }
        ~apply_action:(fun ~inject ~schedule_event dispatcher model action ->
          let writeback a = schedule_event (inject (Set a)) in
          let state = model.state in
          let new_state =
            match action, dispatcher with
            | Activate _, Inactive ->
              (* The activate message got to us, but we became inactive in the interim *)
              state
            | Activate _, Active dispatch ->
              (match state with
               | Initial | State (Disconnected _ | Failed_to_connect _) ->
                 schedule_event (dispatch writeback);
                 State Connecting
               | State (Connecting | Connected) ->
                 (* We got activated, but we're still listening to the previous connection. *)
                 state)
            | Set new_state, Active dispatch ->
              (match new_state with
               | Failed_to_connect _ | Disconnected _ ->
                 (* we failed, but we're still active, so try to reconnect *)
                 schedule_event (dispatch writeback)
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
            let now () = Option.map ~f:Ui_incr.Clock.now clock in
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
