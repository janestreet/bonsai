# RPCs

`Bonsai_web`'s `Rpc_effect` provides effects for dispatching RPCs and
tracking their results.

### Example 1: Doubling a number

In this example, we'll build a simple full-stack app:

-   The client stores an `int` state
-   Whenever a button is clicked, it sends the current value to the
    server
-   The server sends back the input multiplied by 2
-   The client updates its state to the new, doubled number

```{=html}
<iframe data-external="1" src="https://bonsai:8535#double-the-number-rpc">
```
```{=html}
</iframe>
```
The first step is to define an RPC that is shared between the server and
client code.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=protocol-1 -->
```
``` ocaml
let double_rpc =
  Rpc.Rpc.create
    ~name:"double"
    ~version:0
    ~bin_query:[%bin_type_class: int]
    ~bin_response:[%bin_type_class: int]
    ~include_in_error_count:Only_on_exn
;;
```

Next, we provide the server's implementation of the RPC.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=implementation-1 -->
```
``` ocaml
let double_implementation =
  Rpc.Rpc.implement' double_rpc (fun _connection_state query -> Int.max 1 (query * 2))
;;
```

On the client side, we start by picking a server to send our RPCs to.
Most users will pick `Self`, which causes RPCs to be sent to the same
server hosting the web page itself.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=where_to_connect -->
```
``` ocaml
let where_to_connect : Rpc_effect.Where_to_connect.t = Self
```

Finally, we can build the client side of the app.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=client-1 -->
```
``` ocaml
let double_number_app (local_ graph) =
  let dispatch_double_rpc =
    Rpc_effect.Rpc.dispatcher double_rpc ~where_to_connect graph
  in
  let number, set_number = Bonsai.state 1 graph in
  let%arr dispatch_double_rpc = dispatch_double_rpc
  and number = number
  and set_number = set_number in
  Vdom.Node.div
    [ Vdom.Node.div [ Vdom.Node.text [%string "The number is: %{number#Int}"] ]
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              match%bind.Effect dispatch_double_rpc number with
              | Ok doubled_number -> set_number doubled_number
              | Error error -> Effect.of_sync_fun eprint_s [%sexp (error : Error.t)])
          ]
        [ Vdom.Node.text "Double the number" ]
    ]
;;
```

It's worth noting that `dispatch_double_rpc` is a
`query -> 'response unit Effect.t`, so the RPC is only dispatched
whenever the effect is scheduled. Just instantiating
`dispatch_double_rpc` will not send anything.

## Polling State RPC

### Why not use `Pipe_rpc`?

Bonsai apps often use a library called `polling_state_rpc` whenever they
want to keep up to date with a source of data. The library provides a
similar abstraction to the `Pipe_rpc` or `State_rpc` modules that come
built into `async_rpc_kernel`, but with tradeoffs that are more suitable
for web clients.

Both `Pipe_rpc` and `State_rpc` operate by pushing streams of updates
through the connection; the client can then consume this stream of
events as appropriate. This approach works great if the client is able
to keep up with the stream of events, as is often the case with native
programs.

Web UIs are different because they frequently get backgrounded: if the
user switches away from a browser tab, the Bonsai rendering loop, which
relies on the browser's `requestAnimationFrame` function, slows down to
a crawl. If the app uses `State_rpc`, then when the tab eventually comes
back to the foreground, it might have a large queue of events, which
will cause it to freeze while it catches up.

Even more problematically, the server might run out of memory
accumulating massive pipes for backgrounded clients, and crash.

The `polling_state_rpc` library solves this problem by having the client
explicitly poll the server, which responds with a diff from the
previously-requested state to the current data.

### Using Polling State RPC

This next example will build a UI that monitors the current time
reported by the server, in order to provide a concrete example of using
the `polling_state_rpc` library.

```{=html}
<iframe data-external="1" src="https://bonsai:8535#poll-the-current-time">
```
```{=html}
</iframe>
```
Ten times a second, the client requests the current time for some
timezone.

```{=html}
<aside>
```
In a real web UI, you should validate the input you send to the server.
This is omitted here for simplicity.
```{=html}
</aside>
```
As in the previous example, we will begin by setting up the RPC
definition. The point of interest in this case is that we must provide a
module for the response type that satisfies the `Legacy_diffable.S`
module type (from the `legacy_diffable` library).

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=protocol-2 -->
```
``` ocaml
module Current_time = struct
  include String
  include Legacy_diffable.Atomic.Make (String)
end

let current_time_rpc =
  Polling_state_rpc.create
    ~name:"current_time"
    ~version:0
    ~query_equal:[%equal: string]
    ~bin_query:[%bin_type_class: string]
    (module Current_time)
;;
```

Next we provide a server implementation:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=implementation-2 -->
```
``` ocaml
let current_time_implementation =
  Polling_state_rpc.implement
    ~on_client_and_server_out_of_sync:print_s
    current_time_rpc
    (fun _connection_state zone ->
       Deferred.return
         (Time_ns.to_string_trimmed ~zone:(Timezone.of_string zone) (Time_ns.now ())))
  |> Rpc.Implementation.lift ~f:(fun connection_state ->
    connection_state, connection_state)
;;
```

```{=html}
<aside>
```
Using `Rpc.Implementation.lift` is common, because it provides
`'a * Rpc.Connection.t` to the implementation returned by
`Polling_state_rpc.implement`.
```{=html}
</aside>
```
Finally, we can write the UI code. There are a couple important ways in
which this example is different from the previous one:

-   Rather than explicitly dispatching the RPC, we merely instantiate
    the poller and allow it to send requests as necessary. It sends
    requests whenever the query (the timezone) changes, and also at
    fixed intervals (every tenth of a second).
-   Instead of a single `'response Or_error.t`, we get `Bonsai.t`s for
    `last_ok_response` and `last_error`. These are packaged with their
    corresponding query.

The code omits the implementation of `zone_form`, since that is not our
focus in this chapter.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples.ml,part=client-2 -->
```
``` ocaml
let current_time_app (local_ graph) =
  let zone, zone_view = zone_form graph in
  let poll =
    Rpc_effect.Polling_state_rpc.poll
      current_time_rpc
      ~equal_query:[%equal: string]
      ~equal_response:[%equal: Current_time.t]
      ~where_to_connect
      ~every:(Time_ns.Span.of_sec 0.1)
      zone
      graph
  in
  let%arr { last_ok_response; last_error; inflight_query = _; refresh = _ } = poll
  and zone_view = zone_view in
  let text =
    match last_ok_response with
    | Some (zone, current_time) ->
      [%string "The current time in the zone '%{zone}' is %{current_time}"]
    | None -> "Loading..."
  in
  let error_view =
    match last_error with
    | Some (zone, error) ->
      Vdom.Node.div
        ~attrs:[ Css.error_text ]
        [ Vdom.Node.text [%string "Got error when requesting time in zone '%{zone}'"]
        ; Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ]
        ]
    | None -> Vdom.Node.none
  in
  Vdom.Node.div [ zone_view; Vdom.Node.div [ Vdom.Node.text text ]; error_view ]
;;
```

## Testing RPCs

Let's try to test the "number doubler" from the first example. Using the
[testing tools](./testing.mdx) we are familiar with, we might write
something like this:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples_test.ml,part=attempt-1 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) double_number_app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}]
;;
```

No dice. Since RPCs are dispatched asynchronously, the test must itself
be async. Native tests would accomplish this by adding `open Async` at
the top of the test file, but since we're running in javascript, we have
to use a different library.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples_test.ml,part=open-bonsai-web-test-async -->
```
``` ocaml
open Async_kernel
open Bonsai_web_test_async
```

Here's what our attempt looks like now. In addition to the above module
imports, we also use `Handle.flush_async_and_bonsai` to force any
pending side-effects to happen before the test quits. If we don't
include that call, the output still gets printed, but after the
`return ()`, which means the test doesn't typecheck.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples_test.ml,part=attempt-2 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) double_number_app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  let%bind () = Handle.flush_async_and_bonsai handle in
  [%expect {| (Failure "BUG: no bonsai-rpc handler installed") |}];
  return ()
;;
```

We're close, but we need to provide an implementation for the RPC that
the button tried to invoke. Thankfully, `double_implementation` from
earlier is still lying around, so we can simply use that. (In some apps
you might want to include a mock implementation that differs from the
real one.) We should also yield to the scheduler *before* showing the
output, so that we can see the effect of the RPC completing.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/rpc_examples_test.ml,part=attempt-3 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle =
    Handle.create
      ~rpc_implementations:[ double_implementation ]
      (Result_spec.vdom Fn.id)
      double_number_app
  in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  let%bind () = Handle.flush_async_and_bonsai handle in
  Handle.show handle;
  [%expect
    {|
    ------ between bonsai frame ------
    <div>
      <div> The number is: 2 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  return ()
;;
```

Hurrah! This test successfully demonstrates the effect of clicking the
button.

```{=html}
<aside>
```
There are a lot more tricky things about asynchronous testing that this
chapter could cover, but we've opted to stop here. Admittedly, the tools
for testing RPCs are still in their infancy, so if you have ideas about
how to make writing these tests easier, let us know.
```{=html}
</aside>
```
