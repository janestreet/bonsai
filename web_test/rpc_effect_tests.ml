open! Core
open! Async_kernel
open Bonsai_web
open Proc
open Async_rpc_kernel
open Async_js_test

let () = Async_js.init ()
let rpc_a = Rpc.Rpc.create ~name:"a" ~version:0 ~bin_query:bin_int ~bin_response:bin_int
let rpc_b = Rpc.Rpc.create ~name:"b" ~version:0 ~bin_query:bin_int ~bin_response:bin_int
let babel_rpc_a = Babel.Caller.Rpc.singleton rpc_a
let babel_rpc_b = Babel.Caller.Rpc.add babel_rpc_a ~rpc:rpc_b

module Diffable_int = struct
  type t = int [@@deriving sexp, bin_io]

  module Update = struct
    module Diff = struct
      type t = int [@@deriving sexp, bin_io]
    end

    type t = Diff.t list [@@deriving sexp, bin_io]
  end

  let update t diffs = Option.value ~default:t (List.last diffs)

  let diffs ~from ~to_ =
    print_s [%message "Computing diff" (from : int) (to_ : int)];
    [ to_ ]
  ;;

  let to_diffs t = [ t ]
  let of_diffs diffs = Option.value ~default:0 (List.last diffs)
end

let polling_state_rpc =
  Polling_state_rpc.create
    ~name:"polling_state_rpc_a"
    ~version:0
    ~query_equal:[%equal: int]
    ~bin_query:bin_int
    (module Diffable_int)
;;

let async_do_actions handle actions =
  Handle.do_actions handle actions;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

module Int_to_int_or_error = struct
  type t = int -> int Or_error.t Effect.t
  type incoming = int

  let view _ = ""

  let incoming f query =
    let%bind.Effect result = f query in
    Effect.print_s ([%sexp_of: int Or_error.t] result)
  ;;
end

let%expect_test "test fallback" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  let handle = Handle.create (module Int_to_int_or_error) computation in
  (* Invoking the RPC before providing an implementation of it to the handle
     will yield an error as a response. *)
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Error "RPC not handled because no connector has been provided.") |}];
  Deferred.unit
;;

let%expect_test "provided RPC" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  (* By providing an implementation to the handle, we get control over the
     value returned by the RPC. *)
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let%expect_test "not provided RPC" =
  let computation = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  let handle =
    Handle.create ~rpc_implementations:[] (module Int_to_int_or_error) computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect
    {|
   (Error
    ((rpc_error (Unimplemented_rpc a (Version 0)))
     (connection_description <created-directly>) (rpc_name a) (rpc_version 0))) |}];
  Deferred.unit
;;

let%expect_test "latest version of a babel RPC" =
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_a ~where_to_connect:Self in
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let%expect_test "previous version of a babel RPC" =
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~rpc_implementations:[ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind.Deferred () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  Deferred.unit
;;

let incrementing_polling_state_rpc_implementation () =
  let count = ref 0 in
  Rpc.Implementation.lift
    ~f:(fun connection -> connection, connection)
    (Polling_state_rpc.implement
       polling_state_rpc
       ~on_client_and_server_out_of_sync:
         (Expect_test_helpers_core.print_s ~hide_positions:true)
       ~for_first_request:(fun _ query ->
         print_s [%message "For first request" (query : int)];
         return (query * !count))
       (fun _ query ->
          incr count;
          return (query * !count)))
;;

let%expect_test "polling_state_rpc" =
  let computation =
    Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
  in
  let handle =
    Handle.create
      ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 0) |}];
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("Computing diff" (from 0) (to_ 1))
    (Ok 1) |}];
  Deferred.unit
;;

let%expect_test "multiple polling_state_rpc" =
  let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
  let map = Bonsai.Var.value map_var in
  let computation =
    let open Bonsai.Let_syntax in
    Bonsai.assoc
      (module Int)
      map
      ~f:(fun key _data ->
        let%sub dispatcher =
          Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
        in
        let%arr dispatcher = dispatcher
        and key = key in
        dispatcher key)
  in
  let handle =
    Handle.create
      ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
      (module struct
        type t = int Or_error.t Effect.t Int.Map.t
        type incoming = int

        let view _ = ""

        let incoming t query =
          match Map.find t query with
          | Some effect ->
            let%bind.Effect result = effect in
            Effect.print_s ([%sexp_of: int Or_error.t] result)
          | None -> Effect.print_s [%message "Query does not exist in map" (query : int)]
        ;;
      end)
      computation
  in
  (* Since the initial query for each entry of the map does not trigger a diff,
     we know that each one has a different polling_state_rpc client. *)
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 0) |}];
  let%bind () = async_do_actions handle [ 2 ] in
  [%expect {|
    ("For first request" (query 2))
    (Ok 0) |}];
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("For first request" (query 10))
    (Ok 0) |}];
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("Computing diff" (from 0) (to_ 10))
    (Ok 10) |}];
  Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
  Handle.recompute_view handle;
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {| ("Query does not exist in map" (query 10)) |}];
  Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
  Handle.recompute_view handle;
  (* Having been de-activated, this map entry does not trigger a diff
     computation, thus demonstrating that the server probably isn't holding
     onto data about this client.. *)
  let%bind () = async_do_actions handle [ 10 ] in
  [%expect {|
    ("For first request" (query 10))
    (Ok 10) |}];
  Deferred.unit
;;

let create_connection implementations =
  let to_server = Pipe.create () in
  let to_client = Pipe.create () in
  let one_connection implementations pipe_to pipe_from =
    let transport =
      Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
    in
    let%bind conn =
      Rpc.Connection.create ?implementations ~connection_state:Fn.id transport
    in
    return (Result.ok_exn conn)
  in
  don't_wait_for
    (let%bind server_conn =
       one_connection
         (Some (Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Continue))
         to_server
         to_client
     in
     Rpc.Connection.close_finished server_conn);
  let%map connection = one_connection None to_client to_server in
  Or_error.return connection
;;

let%expect_test "disconnect and re-connect async_durable" =
  let is_broken = ref false in
  let implementations =
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ])
  in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () ->
           is_broken := false;
           create_connection !implementations)
         ~is_broken:(fun _ -> !is_broken)
         ())
  in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  is_broken := true;
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 1) |}];
  return ()
;;

let%expect_test "disconnect and re-connect persistent_connection" =
  let module Conn =
    Persistent_connection_kernel.Make (struct
      type t = Rpc.Connection.t

      let close t = Rpc.Connection.close t
      let is_closed t = Rpc.Connection.is_closed t
      let close_finished t = Rpc.Connection.close_finished t
    end)
  in
  let implementations =
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ])
  in
  let connection =
    Conn.create
      ~server_name:"test_server"
      ~connect:(fun () -> create_connection !implementations)
      ~address:(module Unit)
      (fun () -> Deferred.Or_error.return ())
  in
  let connector = Rpc_effect.Connector.persistent_connection (module Conn) connection in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 0) |}];
  let%bind () =
    let connection = Option.value_exn (Conn.current_connection connection) in
    let%bind () = Rpc.Connection.close connection in
    Rpc.Connection.close_finished connection
  in
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind _connection = Conn.connected connection in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 1) |}];
  return ()
;;

let%expect_test "connect without menu" =
  let is_broken = ref false in
  let implementations = ref [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ] in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () ->
           is_broken := false;
           create_connection !implementations)
         ~is_broken:(fun _ -> !is_broken)
         ())
  in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect
    {|
    (Error
     ((rpc_error (Unimplemented_rpc __Versioned_rpc.Menu (Version 1)))
      (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
      (rpc_version 1))) |}];
  is_broken := true;
  implementations := [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect
    {|
    (Error
     ((rpc_error (Unimplemented_rpc __Versioned_rpc.Menu (Version 1)))
      (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
      (rpc_version 1))) |}];
  is_broken := true;
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' rpc_b (fun _ _query -> 1) ];
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect {| (Ok 1) |}];
  return ()
;;

let%expect_test "menu rpc request fails" =
  let implementations =
    Versioned_rpc.Menu.implement_multi (fun _ ~version:_ () ->
      print_endline "executed menu rpc";
      raise_s [%message "menu rpc failed"])
    @ [ Rpc.Rpc.implement' rpc_a (fun _ _query -> 0) ]
  in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () -> create_connection implementations)
         ~is_broken:(fun _ -> false)
         ())
  in
  let computation = Rpc_effect.Rpc.babel_dispatcher babel_rpc_b ~where_to_connect:Self in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 0 ] in
  [%expect
    {|
    executed menu rpc
    (Error
     ((rpc_error
       (Uncaught_exn
        ((location "server-side rpc computation")
         (exn (monitor.ml.Error "menu rpc failed")))))
      (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
      (rpc_version 1))) |}];
  let%bind () = async_do_actions handle [ 0 ] in
  (* The crucial part of this test is that the implementation of the menu RPC
     runs twice, which demonstrates that errors when fetching the menu aren't
     cached, even successes are. *)
  [%expect
    {|
    executed menu rpc
    (Error
     ((rpc_error
       (Uncaught_exn
        ((location "server-side rpc computation")
         (exn (monitor.ml.Error "menu rpc failed")))))
      (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
      (rpc_version 1))) |}];
  return ()
;;

let%expect_test "disconnect and re-connect with polling_state_rpc" =
  let module Conn =
    Persistent_connection_kernel.Make (struct
      type t = Rpc.Connection.t

      let close t = Rpc.Connection.close t
      let is_closed t = Rpc.Connection.is_closed t
      let close_finished t = Rpc.Connection.close_finished t
    end)
  in
  let implementations = [ incrementing_polling_state_rpc_implementation () ] in
  let connection =
    Conn.create
      ~server_name:"test_server"
      ~connect:(fun () -> create_connection implementations)
      ~address:(module Unit)
      (fun () -> Deferred.Or_error.return ())
  in
  let connector = Rpc_effect.Connector.persistent_connection (module Conn) connection in
  let computation =
    Rpc_effect.Polling_state_rpc.dispatcher polling_state_rpc ~where_to_connect:Self
  in
  let handle =
    Handle.create
      ~connectors:(fun _ -> connector)
      (module Int_to_int_or_error)
      computation
  in
  let%bind () = async_do_actions handle [ 1 ] in
  [%expect {|
    ("For first request" (query 1))
    (Ok 0) |}];
  let%bind () =
    let connection = Option.value_exn (Conn.current_connection connection) in
    let%bind () = Rpc.Connection.close connection in
    Rpc.Connection.close_finished connection
  in
  let%bind _connection = Conn.connected connection in
  let%bind () = async_do_actions handle [ 2 ] in
  [%expect {|
    ("For first request" (query 2))
    (Ok 0) |}];
  return ()
;;

let%test_module "Rvar tests" =
  (module struct
    module Rvar = Rpc_effect.Private.For_tests.Rvar

    let%expect_test _ =
      let i = ref 0 in
      let rec t =
        lazy
          (Rvar.create (fun () ->
             incr i;
             print_s [%message "iteration" (!i : int)];
             if !i < 10 then Rvar.invalidate (Lazy.force t);
             Deferred.Or_error.return !i))
      in
      let%bind () =
        match%map Rvar.contents (Lazy.force t) with
        | Ok x -> print_s [%message "final result" (x : int)]
        | Error e -> print_s [%message (e : Error.t)]
      in
      [%expect
        {|
        (iteration (!i 1))
        (iteration (!i 2))
        (iteration (!i 3))
        (iteration (!i 4))
        (iteration (!i 5))
        (iteration (!i 6))
        (iteration (!i 7))
        (iteration (!i 8))
        (iteration (!i 9))
        (iteration (!i 10))
        ("final result" (x 10)) |}];
      return ()
    ;;
  end)
;;

let%test_module "Status.state" =
  (module struct
    module Conn = Persistent_connection_kernel.Make (struct
        type t = Rpc.Connection.t

        let close t = Rpc.Connection.close t
        let is_closed t = Rpc.Connection.is_closed t
        let close_finished t = Rpc.Connection.close_finished t
      end)

    module Status_option = struct
      type t = Rpc_effect.Status.t option [@@deriving sexp_of]
    end

    let kill_connection connection =
      let%bind () =
        connection |> Conn.current_connection |> Option.value_exn |> Rpc.Connection.close
      in
      Async_kernel_scheduler.yield_until_no_jobs_remain ()
    ;;

    let next_connection connection =
      let%bind _connection = Conn.connected connection in
      Async_kernel_scheduler.yield_until_no_jobs_remain ()
    ;;

    let make_connection_and_connector () =
      let connection =
        Conn.create
          ~server_name:"test_server"
          ~connect:(fun () -> create_connection [])
          ~address:(module Unit)
          (fun () -> Deferred.Or_error.return ())
      in
      let connector =
        Rpc_effect.Connector.persistent_connection (module Conn) connection
      in
      connection, connector
    ;;

    let%expect_test "basic usage" =
      let connection, connector = make_connection_and_connector () in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Rpc_effect.Status))
          (Rpc_effect.Status.state ~where_to_connect:Self)
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      let%bind () = kill_connection connection in
      Handle.show handle;
      [%expect
        {|
        ((state (Disconnected Rpc.Connection.close))
         (connecting_since ("1970-01-01 00:00:00Z"))) |}];
      let%bind () = next_connection connection in
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      return ()
    ;;

    let%expect_test "connecting-since" =
      let connection, connector = make_connection_and_connector () in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Rpc_effect.Status))
          (Rpc_effect.Status.state ~where_to_connect:Self)
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:01Z"))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      let%bind () = kill_connection connection in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect
        {|
        ((state (Disconnected Rpc.Connection.close))
         (connecting_since ("1970-01-01 00:00:03Z"))) |}];
      let%bind () = next_connection connection in
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      Handle.show handle;
      [%expect {| ((state Connected) (connecting_since ())) |}];
      return ()
    ;;

    let%expect_test "closing happens when component is inactive" =
      let connection, connector = make_connection_and_connector () in
      let is_active = Bonsai.Var.create true in
      let component =
        let open Bonsai.Let_syntax in
        if%sub Bonsai.Var.value is_active
        then (
          let%sub status = Rpc_effect.Status.state ~where_to_connect:Self in
          Bonsai.pure Option.some status)
        else Bonsai.const None
      in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Status_option))
          component
      in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ()))) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      Bonsai.Var.set is_active false;
      Handle.show handle;
      [%expect {| () |}];
      let%bind () = kill_connection connection in
      Handle.show handle;
      [%expect
        {|
        ("an action inside of Bonsai.switch has been dropped because the computation is no longer active"
         (index 1) (action (Set (Disconnected Rpc.Connection.close))))
        () |}];
      Bonsai.Var.set is_active true;
      Handle.show handle;
      [%expect
        {|
        (((state (Disconnected Rpc.Connection.close))
          (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      let%bind () = next_connection connection in
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      return ()
    ;;

    let%expect_test "opening happens when component is inactive" =
      let _connection, connector = make_connection_and_connector () in
      let is_active = Bonsai.Var.create true in
      let component =
        let open Bonsai.Let_syntax in
        if%sub Bonsai.Var.value is_active
        then (
          let%sub status = Rpc_effect.Status.state ~where_to_connect:Self in
          Bonsai.pure Option.some status)
        else Bonsai.const None
      in
      let handle =
        Handle.create
          ~connectors:(fun _ -> connector)
          (Result_spec.sexp (module Status_option))
          component
      in
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ()))) |}];
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| (((state Connecting) (connecting_since ("1970-01-01 00:00:00Z")))) |}];
      Bonsai.Var.set is_active false;
      Handle.show handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect
        {|
        ()
        ("an action inside of Bonsai.switch has been dropped because the computation is no longer active"
         (index 1) (action (Set Connected)))
        () |}];
      Bonsai.Var.set is_active true;
      Handle.show handle;
      [%expect {| (((state Connected) (connecting_since ()))) |}];
      return ()
    ;;

    let%expect_test "failed to connect" =
      let component = Rpc_effect.Status.state ~where_to_connect:Self in
      let handle =
        Handle.create (Result_spec.sexp (module Rpc_effect.Status)) component
      in
      Handle.show handle;
      [%expect {| ((state Connecting) (connecting_since ())) |}];
      Handle.recompute_view_until_stable handle;
      let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
      Handle.show handle;
      [%expect
        {|
          ((state
            (Failed_to_connect
             "RPC not handled because no connector has been provided."))
           (connecting_since ("1970-01-01 00:00:00Z"))) |}];
      return ()
    ;;
  end)
;;

let%test_module "Polling_state_rpc.poll" =
  (module struct
    let async_show handle =
      Handle.show handle;
      Async_kernel_scheduler.yield_until_no_jobs_remain ()
    ;;

    let%expect_test "basic usage" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          (module Int)
          (module Int)
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Polling_state_rpc.Result.t [@@deriving sexp]
             end))
          computation
      in
      let%bind () = async_show handle in
      (* Initially, there is no response, but initial request got sent. *)
      [%expect {|
        No_responses_yet
        ("For first request" (query 1)) |}];
      let%bind () = async_show handle in
      (* Because the clock triggers on activate, the next frame both receives the
         first request's response and also sets off the first polling request. *)
      [%expect
        {|
         (Last_response_was_ok (query 1) (response 0))
         ("Computing diff" (from 0) (to_ 1)) |}];
      let%bind () = async_show handle in
      (* The result stays steady this frame, and not new requests are sent off. *)
      [%expect {| (Last_response_was_ok (query 1) (response 1)) |}];
      Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
      let%bind () = async_show handle in
      (* After waiting a second, apparently the clock loop needs another frame
         to realize that its time is up. *)
      [%expect {| (Last_response_was_ok (query 1) (response 1)) |}];
      let%bind () = async_show handle in
      (* But it eventually causes the next polling request to be sent. *)
      [%expect
        {|
         (Last_response_was_ok (query 1) (response 1))
         ("Computing diff" (from 1) (to_ 2)) |}];
      let%bind () = async_show handle in
      [%expect {| (Last_response_was_ok (query 1) (response 2)) |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      (* We also trigger poll requests on query changes. Observe that the
         response includes the query that was used to make the response, which
         in this case is different from the current query. *)
      [%expect
        {|
         (Last_response_was_ok (query 1) (response 2))
         ("For first request" (query 2))
         ("Computing diff" (from 2) (to_ 4)) |}];
      let%bind () = async_show handle in
      [%expect {| (Last_response_was_ok (query 2) (response 4)) |}];
      Deferred.unit
    ;;

    let every_other_error_polling_state_rpc_implementation () =
      let count = ref 0 in
      let next_response_is_error_ref = ref true in
      let next_result query =
        let next_response_is_error = !next_response_is_error_ref in
        next_response_is_error_ref := not next_response_is_error;
        let result =
          if next_response_is_error
          then raise_s [%message "Error response" (query : int)]
          else query * !count
        in
        return result
      in
      Rpc.Implementation.lift
        ~f:(fun connection -> connection, connection)
        (Polling_state_rpc.implement
           polling_state_rpc
           ~on_client_and_server_out_of_sync:
             (Expect_test_helpers_core.print_s ~hide_positions:true)
           ~for_first_request:(fun _ query -> next_result query)
           (fun _ query ->
              incr count;
              next_result query))
    ;;

    let%expect_test "hit all possible responses from the poller" =
      let input_var = Bonsai.Var.create 1 in
      let computation =
        Rpc_effect.Polling_state_rpc.poll
          (module Int)
          (module Int)
          polling_state_rpc
          ~where_to_connect:Self
          ~every:(Time_ns.Span.of_sec 1.0)
          (Bonsai.Var.value input_var)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ every_other_error_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Polling_state_rpc.Result.t [@@deriving sexp]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect {| No_responses_yet |}];
      let%bind () = async_show handle in
      [%expect
        {|
         (Error_before_any_ok_responses
          (error
           ((rpc_error
             (Uncaught_exn
              ((location "server-side rpc computation")
               (exn (monitor.ml.Error ("Error response" (query 1)))))))
            (connection_description <created-directly>) (rpc_name polling_state_rpc_a)
            (rpc_version 0)))
          (query 1)) |}];
      let%bind () = async_show handle in
      [%expect {| (Last_response_was_ok (query 1) (response 0)) |}];
      Bonsai.Var.set input_var 2;
      let%bind () = async_show handle in
      [%expect {| (Last_response_was_ok (query 1) (response 0)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        (Error_after_last_ok_response (query 2)
         (error
          ((rpc_error
            (Uncaught_exn
             ((location "server-side rpc computation")
              (exn (monitor.ml.Error ("Error response" (query 2)))))))
           (connection_description <created-directly>) (rpc_name polling_state_rpc_a)
           (rpc_version 0)))
         (last_ok_query 1) (last_ok_response 0)) |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers from, clear on deactivate (on by default)" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Polling_state_rpc.poll
              (module Int)
              (module Int)
              polling_state_rpc
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Polling_state_rpc.Result.t Int.Map.t
               [@@deriving sexp]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 No_responses_yet) (2 No_responses_yet) (10 No_responses_yet))
        ("For first request" (query 2))
        ("For first request" (query 1))
        ("For first request" (query 10)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
         ((1 (Last_response_was_ok (query 1) (response 0)))
          (2 (Last_response_was_ok (query 2) (response 0)))
          (10 (Last_response_was_ok (query 10) (response 0))))
         ("Computing diff" (from 0) (to_ 1))
         ("Computing diff" (from 0) (to_ 4))
         ("Computing diff" (from 0) (to_ 30)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))
         (10 (Last_response_was_ok (query 10) (response 30)))) |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))) |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we clear the map entry when it gets de-activated, it does not
         remember its last response, and thus must poll for it again. *)
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4))) (10 No_responses_yet))
        ("For first request" (query 10)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))
         (10 (Last_response_was_ok (query 10) (response 30))))
        ("Computing diff" (from 30) (to_ 40)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))
         (10 (Last_response_was_ok (query 10) (response 40)))) |}];
      Deferred.unit
    ;;

    let%expect_test "multiple pollers from, don't clear on deactivate" =
      let map_var = Bonsai.Var.create (Int.Map.of_alist_exn [ 1, (); 2, (); 10, () ]) in
      let map = Bonsai.Var.value map_var in
      let computation =
        Bonsai.assoc
          (module Int)
          map
          ~f:(fun key _data ->
            Rpc_effect.Polling_state_rpc.poll
              (module Int)
              (module Int)
              polling_state_rpc
              ~clear_when_deactivated:false
              ~where_to_connect:Self
              ~every:(Time_ns.Span.of_sec 1.0)
              key)
      in
      let handle =
        Handle.create
          ~rpc_implementations:[ incrementing_polling_state_rpc_implementation () ]
          (Result_spec.sexp
             (module struct
               type t = (int, int) Rpc_effect.Polling_state_rpc.Result.t Int.Map.t
               [@@deriving sexp]
             end))
          computation
      in
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 No_responses_yet) (2 No_responses_yet) (10 No_responses_yet))
        ("For first request" (query 2))
        ("For first request" (query 1))
        ("For first request" (query 10)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
         ((1 (Last_response_was_ok (query 1) (response 0)))
          (2 (Last_response_was_ok (query 2) (response 0)))
          (10 (Last_response_was_ok (query 10) (response 0))))
         ("Computing diff" (from 0) (to_ 1))
         ("Computing diff" (from 0) (to_ 4))
         ("Computing diff" (from 0) (to_ 30)) |}];
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))
         (10 (Last_response_was_ok (query 10) (response 30)))) |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.remove map 10);
      let%bind () = async_show handle in
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))) |}];
      Bonsai.Var.update map_var ~f:(fun map -> Map.set map ~key:10 ~data:());
      let%bind () = async_show handle in
      (* since we do not clear the map entry when it gets de-activated, it does
         remember its last response, and thus does not need to poll for it again. *)
      [%expect
        {|
        ((1 (Last_response_was_ok (query 1) (response 1)))
         (2 (Last_response_was_ok (query 2) (response 4)))
         (10 (Last_response_was_ok (query 10) (response 30)))) |}];
      Deferred.unit
    ;;
  end)
;;
