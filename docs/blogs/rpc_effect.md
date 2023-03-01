# Ergonomic and testable RPC dispatching in web clients

Historically, Bonsai and Async haven't gotten along too well. Because Bonsai
only works with pure programs and immutable values, the side-effectful and
mutable nature of Async operations meant that the best architecture for web apps
involved splitting the program into "pure bonsai" and "impure async" worlds. These
two worlds were then connected by using specific APIs, like `Effect.of_deferred_fun`
(which turns an Deferred-returning function into an Effect-returning function) and
`Bonsai.Var.t` (which is used to track live data and inject it into Bonsai)

The split was certainly annoying, but it did come with a side-benefit: pure Bonsai
components are really easy to test because mocking out communication with the outside
world is really easy.

This year the team has been busy making Bonsai powerful enough to robustly deal with
mutable values and impure functions, culminating in `Bonsai_web.Rpc_effect`, a module
exposing first-class Bonsai components for performing RPCs, tracking their responses,
and monitoring the status of your connections.

Highlights of the [module's mli](https://cs/jane/lib/bonsai/web/rpc_effect.mli?rev=c8ebb2571ed3587002aede37188e324ab6b3406f)
include:

<!-- These item titles are intentionally weird and barely related to the content
because any title I could think of that was closer to the content would have
made things more repetitive. -->
- **Boilerplate-free:** Connection information is passed implicitly through the
  Bonsai computation graph, which means you don't need to thread rpc-sending
  effect parameters (or a `Connection.t`) through your whole app.
- **Powerful combinators:** `Polling_state_rpc.poll` turns a polling state-RPC
   into a Bonsai component which automatically fetches new data when an input
   `'query Value.t` changes (and on a timer so that the server can forward updates).
- **No compromises for testability:** Testing components that use this module
  is as easy as providing implementations for any of the RPCs invoked during
  each test. If your server's RPC implementations are in a `js_of_ocaml`-compatible
  library, you could even use those for tests.
- **Helpful utilities:** `Status.state` is a computation for tracking whether the
  UI is connected to its server. If it has been disconnected, it also tracks
  how long ago it was last connected.

# Migrating an existing app

Suppose you have a Bonsai app whose main computation has the following type:

```ocaml
val app
  :  counters:int Map.M(String).t Value.t
  -> send_increment_rpc:(int -> unit Or_error.t Effect.t)
  -> send_decrement_rpc:(int -> unit Or_error.t Effect.t)
  -> Vdom.Node.t Computation.t
```

To migrate this signature to use the `Rpc_effect` module, follow these steps.
The diffs below include just the essense of the migration; merely making these
changes will yield type errors that should be straightforward enough to
address.

## Step 1 - Move all RPC dispatches into the main Bonsai computation.

The goal in this step is to remove all the parameters to `app`.

```diff
 let app
-    ~counters
     ~send_increment_rpc
     ~send_decrement_rpc
   =
+  let%sub counters =
+    Rpc_effect.Polling_state_rpc.poll
+      (module Unit)
+      (module Counter_state)
+      Protocol.Counter_state.t
+      ~where_to_connect:Self
+      ~every:(Time_ns.Span.of_ms 16.0)
+      (Value.return ())
+  in
```

The new `counters` variable has a different type than the old one, since
`Rpc_effect.Polling_state_rpc.poll` has extra cases for handling when no
response has yes been received or when the last response was an error.

```diff
 let app
-    ~send_increment_rpc
-    ~send_decrement_rpc
     =
+    let%sub send_decrement_rpc =
+      Rpc_effect.Rpc.dispatcher Protocol.Decrement_request.t ~where_to_connect:Self
+    in
+    let%sub send_increment_rpc =
+      Rpc_effect.Rpc.dispatcher Protocol.Increment_request.t ~where_to_connect:Self
+    in
```

## Step 2 - Delete the old connection logic.

The `Rpc_effect` module creates its own connection for sending RPCs to `Self`.
Thus, we should get rid of the old logic that connects and sends RPC
connections. While most of the diff below consists of removing code made unused
by previous changes, we include it all to demonstrate how much untestable code
we get to remove. In addition, we also now get an opportunity to replace the
`eprint_s` call below with a better error handling mechanism, such as giving
the user a notification that they aren't connected to the server.

```diff
-let counters conn =
-  let client = Polling_state_rpc.Client.create Protocol.Counter_state.t in
-  let%map.Deferred.Or_error first_response =
-    let%bind conn = Rpc_connection.connected conn in
-    Polling_state_rpc.Client.dispatch client conn ()
-  in
-  let world_state_var = Bonsai.Var.create first_response in
-  Async_kernel.Clock_ns.every'
-    ~continue_on_error:true
-    (Time_ns.Span.of_sec (1.0 /. 30.0))
-    (fun () ->
-       match%map
-         let%bind conn = Rpc_connection.connected conn in
-         Polling_state_rpc.Client.dispatch client conn ()
-       with
-       | Ok () -> ()
-       | Error error ->
-         eprint_s [%message "Failed to poll counter state" (error : Error.t)]);
-  let (_ : _ Bus.Subscriber.t) =
-    Bus.subscribe_exn (Polling_state_rpc.Client.bus client) [%here] ~f:(fun _ ->
-      Bonsai.Var.set world_state_var)
-  in
-  Bonsai.Var.value world_state_var
-;;
-
-let send_increment_rpc conn =
-  Effect.of_deferred_fun (fun how_much ->
-    let%bind conn = Rpc_connection.connected conn in
-    Rpc.Rpc.dispatch Protocol.Increment_request.t conn how_much)
-;;
-
-let send_decrement_rpc conn =
-  Effect.of_deferred_fun (fun how_much ->
-    let%bind conn = Rpc_connection.connected conn in
-    Rpc.Rpc.dispatch Protocol.Increment_request.t conn how_much)
-;;

 let run () =
   Async_js.init ();
-  let conn =
-    Rpc_connection.create
-      ~server_name:"ws-server"
-      ~connect:(fun () -> Rpc.Connection.client ())
-      ~address:(module Unit)
-      Deferred.Or_error.return
-  in
-  let%bind counters = counters conn |> Deferred.Or_error.ok_exn in
-  let counters = Bonsai.Value.map counters ~f:Counter_state.counters in
-  let (_ : (unit, Nothing.t) Start.Handle.t) =
-    Start.start
-      Start.Result_spec.just_the_view
-      ~bind_to_element_with_id:"app"
-      (App.app
-         ~counters
-         ~send_increment_rpc:(send_increment_rpc conn)
-         ~send_decrement_rpc:(send_decrement_rpc conn))
+  let (_ : (unit, Nothing.t) Start.Handle.t) =
+    Bonsai_web.Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" App.app
   in
   Deferred.never ()
```

## Step 3 - Provide RPC implementations for tests

Since `Rpc_effect` causes tests to use `Async` machinery, we have to include
this line near the top of each test file.

```ocaml
open! Async_js_test
```

The main changes to test code include:

- Pass RPC implementations to `Handle.create` instead of passing effect
  implementations to the app computation.
- Use `yield_until_no_jobs_remain` to flush the async scheduler so that
  side-effects happen at the time you expect.
- Use `return ()` at the end of your test, since the test uses async.

```diff
 let%expect_test "Click on the buttons" =
   let handle =
     Handle.create
-      (Result_spec.vdom Fn.id)
-      (App.app
-         ~counters:(Value.return String.Map.empty)
-         ~send_increment_rpc:(fun how_much ->
-           let%map.Effect () = Effect.print_s [%message "incremented" (how_much : int)] in
-           Ok ())
-         ~send_decrement_rpc:(fun how_much ->
-           let%map.Effect () = Effect.print_s [%message "decremented" (how_much : int)] in
-           Ok ()))
+      ~rpc_implementations:
+        [ Rpc.Rpc.implement' Protocol.Increment_request.t (fun _state how_much ->
+            print_s [%message "incremented" (how_much : int)])
+        ; Rpc.Rpc.implement' Protocol.Decrement_request.t (fun _state how_much ->
+            print_s [%message "decremented" (how_much : int)])
+        ]
+      (Result_spec.vdom Fn.id)
+      App.app
   in
   Handle.click_on handle ~get_vdom:Fn.id ~selector:"button.increment";
+  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
   Handle.click_on handle ~get_vdom:Fn.id ~selector:"button.decrement";
+  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
   [%expect {|
     (incremented (how_much 1))
-    (decremented (how_much 1)) |}]
+    (decremented (how_much 1)) |}];
+  return ()
 ;;
```

# Unconventional apps

The assumption built into the steps above is that your app uses a single
`Persistent_connection` to the host server through which all RPCs are sent. We
have also assumed that there is no special connection logic.

For apps that don't fit this pattern, the usage of `Rpc_effect` is mostly the
same, except that you have to specify the connection yourself, rather than
relying on the one created by `Rpc_effect` itself.

## Step 1 - Extend the `Where_to_connect.Custom` variant.

This variant case is the constructor used by the code that dispatches the RPC
and also the code that provides the connection (as you'll see later).

```ocaml
type Rpc_effect.Where_to_connect.Custom.t += Conn
```

## Step 2 - Use the new variant case whenever dispatching an RPC

In the previous example, we used `Self` for all the `~where_to_connect`
parameters, but since we're providing our own connection, we use `Custom Conn`.

```diff
     let%sub send_decrement_rpc =
-      Rpc_effect.Rpc.dispatcher Protocol.Decrement_request.t ~where_to_connect:Self
+      Rpc_effect.Rpc.dispatcher Protocol.Decrement_request.t ~where_to_connect:(Custom Conn)
     in
     let%sub send_decrement_rpc =
-      Rpc_effect.Rpc.dispatcher Protocol.Decrement_request.t ~where_to_connect:Self
+      Rpc_effect.Rpc.dispatcher Protocol.Decrement_request.t ~where_to_connect:(Custom Conn)
     in
```

## Step 3 - Provide your connection using a `Connector.t`

The `Connector` module abstracts over different ways of obtaining connections,
including `async_durable` and `persistent_connection`.

In the app's startup code, provide a connector constructed with either
`Rpc_effect.Connector.async_durable` or `Rpc_effect.Connector.persistent_connection`.

```ocaml
let run () =
  let conn =
    Rpc_connection.create
      ~server_name:"ws-server"
      ~connect:(fun () -> Rpc.Connection.client ())
      ~address:(module Unit)
      Deferred.Or_error.return
  in
  let (_ : (unit, Nothing.t) Start.Handle.t) =
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      ~custom_connector:(function
        | Conn -> Rpc_effect.Connector.persistent_connection (module Rpc_connection) conn
        | _ -> assert false)
      App.app
  in
  Deferred.never ()
```
