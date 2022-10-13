open! Core
open! Async_kernel
open Bonsai_web
open Proc
open Async_rpc_kernel
open Async_js_test

let () = Async_js.init ()

let test_rpc_a =
  Rpc.Rpc.create ~name:"a" ~version:0 ~bin_query:bin_unit ~bin_response:bin_int
;;

let test_rpc_b =
  Rpc.Rpc.create ~name:"b" ~version:0 ~bin_query:bin_unit ~bin_response:bin_int
;;

let test_babel_rpc_a = Babel.Caller.Rpc.singleton test_rpc_a
let test_babel_rpc_b = Babel.Caller.Rpc.add test_babel_rpc_a ~rpc:test_rpc_b

let component =
  let open Bonsai.Let_syntax in
  let%sub dispatch_a = Rpc_effect.Rpc.dispatcher test_rpc_a ~where_to_connect:Self in
  let%sub dispatch_b = Rpc_effect.Rpc.dispatcher test_rpc_b ~where_to_connect:Self in
  let%sub dispatch_c =
    Rpc_effect.Rpc.babel_dispatcher test_babel_rpc_a ~where_to_connect:Self
  in
  let%sub dispatch_d =
    Rpc_effect.Rpc.babel_dispatcher test_babel_rpc_b ~where_to_connect:Self
  in
  let%arr dispatch_a = dispatch_a
  and dispatch_b = dispatch_b
  and dispatch_c = dispatch_c
  and dispatch_d = dispatch_d in
  let rpc_button ~id dispatch_rpc =
    Vdom.Node.button
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.on_click (fun _ ->
               match%bind.Effect dispatch_rpc () with
               | Ok data ->
                 Effect.print_s [%message "RPC finished successfully." (data : int)]
               | Error e -> Effect.print_s ([%sexp_of: Error.t] e))
           ; Vdom.Attr.id id
           ])
      []
  in
  Vdom.Node.div
    [ rpc_button ~id:"a" dispatch_a
    ; rpc_button ~id:"b" dispatch_b
    ; rpc_button ~id:"c" dispatch_c
    ; rpc_button ~id:"d" dispatch_d
    ]
;;

let rpc_implementations = [ Rpc.Rpc.implement' test_rpc_a (fun _ () -> 0) ]

let create_handle ?rpc_implementations component =
  Handle.create ?rpc_implementations (Result_spec.vdom Fn.id) component
;;

let async_click_on handle selector =
  Handle.click_on handle ~get_vdom:Fn.id ~selector;
  Async_kernel_scheduler.yield_until_no_jobs_remain ()
;;

let%expect_test "the appearance of the test component" =
  let handle = create_handle component in
  Handle.show handle;
  [%expect
    {|
    <div>
      <button id="a" onclick> </button>
      <button id="b" onclick> </button>
      <button id="c" onclick> </button>
      <button id="d" onclick> </button>
    </div> |}];
  Deferred.return ()
;;

let%expect_test "test fallback" =
  let handle = create_handle component in
  (* Invoking the RPC before providing an implementation of it to the handle
     will yield an error as a response. *)
  let%bind.Deferred () = async_click_on handle "#a" in
  [%expect {| "RPC not handled because no connector has been provided." |}];
  Deferred.return ()
;;

let%expect_test "provided RPC" =
  (* By providing an implementation to the handle, we get control over the
     value returned by the RPC. *)
  let handle = create_handle ~rpc_implementations component in
  let%bind.Deferred () = async_click_on handle "#a" in
  [%expect {| ("RPC finished successfully." (data 0)) |}];
  Deferred.return ()
;;

let%expect_test "not provided RPC" =
  let handle = create_handle ~rpc_implementations component in
  let%bind.Deferred () = async_click_on handle "#b" in
  [%expect
    {|
   ((rpc_error (Unimplemented_rpc b (Version 0)))
    (connection_description <created-directly>) (rpc_name b) (rpc_version 0)) |}];
  Deferred.return ()
;;

let%expect_test "latest version of a babel RPC" =
  let handle = create_handle ~rpc_implementations component in
  let%bind.Deferred () = async_click_on handle "#c" in
  [%expect {| ("RPC finished successfully." (data 0)) |}];
  Deferred.return ()
;;

let%expect_test "previous version of a babel RPC" =
  let handle = create_handle ~rpc_implementations component in
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 0)) |}];
  Deferred.return ()
;;

let create_connection implementations =
  let to_server = Pipe.create () in
  let to_client = Pipe.create () in
  let one_connection implementations pipe_to pipe_from =
    let transport =
      Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
    in
    let%bind conn =
      Rpc.Connection.create ?implementations ~connection_state:(fun _ -> ()) transport
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
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' test_rpc_a (fun _ () -> 0) ])
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
  let handle =
    Handle.create ~connectors:(fun _ -> connector) (Result_spec.vdom Fn.id) component
  in
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 0)) |}];
  is_broken := true;
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' test_rpc_b (fun _ () -> 1) ];
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 1)) |}];
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
    ref (Versioned_rpc.Menu.add [ Rpc.Rpc.implement' test_rpc_a (fun _ () -> 0) ])
  in
  let connection =
    Conn.create
      ~server_name:"test_server"
      ~connect:(fun () -> create_connection !implementations)
      ~address:(module Unit)
      (fun () -> Deferred.Or_error.return ())
  in
  let connector = Rpc_effect.Connector.persistent_connection (module Conn) connection in
  let handle =
    Handle.create ~connectors:(fun _ -> connector) (Result_spec.vdom Fn.id) component
  in
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 0)) |}];
  let%bind () =
    let connection = Option.value_exn (Conn.current_connection connection) in
    let%bind () = Rpc.Connection.close connection in
    Rpc.Connection.close_finished connection
  in
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' test_rpc_b (fun _ () -> 1) ];
  let%bind _connection = Conn.connected connection in
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 1)) |}];
  return ()
;;

let%expect_test "connect without menu" =
  let is_broken = ref false in
  let implementations = ref [ Rpc.Rpc.implement' test_rpc_a (fun _ () -> 0) ] in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () ->
           is_broken := false;
           create_connection !implementations)
         ~is_broken:(fun _ -> !is_broken)
         ())
  in
  let handle =
    Handle.create ~connectors:(fun _ -> connector) (Result_spec.vdom Fn.id) component
  in
  let%bind () = async_click_on handle "#d" in
  [%expect
    {|
    ((rpc_error (Unimplemented_rpc __Versioned_rpc.Menu (Version 1)))
     (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
     (rpc_version 1)) |}];
  is_broken := true;
  implementations := [ Rpc.Rpc.implement' test_rpc_b (fun _ () -> 1) ];
  let%bind () = async_click_on handle "#d" in
  [%expect
    {|
    ((rpc_error (Unimplemented_rpc __Versioned_rpc.Menu (Version 1)))
     (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
     (rpc_version 1)) |}];
  is_broken := true;
  implementations
  := Versioned_rpc.Menu.add [ Rpc.Rpc.implement' test_rpc_b (fun _ () -> 1) ];
  let%bind () = async_click_on handle "#d" in
  [%expect {| ("RPC finished successfully." (data 1)) |}];
  return ()
;;

let%expect_test "menu rpc request fails" =
  let implementations =
    Versioned_rpc.Menu.implement_multi (fun _ ~version:_ () ->
      print_endline "executed menu rpc";
      assert false)
    @ [ Rpc.Rpc.implement' test_rpc_a (fun _ () -> 0) ]
  in
  let connector =
    Rpc_effect.Connector.async_durable
      (Async_durable.create
         ~to_create:(fun () -> create_connection implementations)
         ~is_broken:(fun _ -> false)
         ())
  in
  let handle =
    Handle.create ~connectors:(fun _ -> connector) (Result_spec.vdom Fn.id) component
  in
  let%bind () = async_click_on handle "#d" in
  [%expect
    {|
    executed menu rpc
    ((rpc_error
      (Uncaught_exn
       ((location "server-side rpc computation")
        (exn (monitor.ml.Error "Assert_failure rpc_effect_tests.ml:275:6")))))
     (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
     (rpc_version 1)) |}];
  let%bind () = async_click_on handle "#d" in
  (* The crucial part of this test is that the implementation of the menu RPC
     runs twice, which demonstrates that errors when fetching the menu aren't
     cached, even successes are. *)
  [%expect
    {|
    executed menu rpc
    ((rpc_error
      (Uncaught_exn
       ((location "server-side rpc computation")
        (exn (monitor.ml.Error "Assert_failure rpc_effect_tests.ml:275:6")))))
     (connection_description <created-directly>) (rpc_name __Versioned_rpc.Menu)
     (rpc_version 1)) |}];
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
