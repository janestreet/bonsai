# 11 - RPCs

Historically, the recommended structure of a web app app that sends RPCs
was something like this:

1.  Use `persistent_connection` to establish a connection the server and
    client can use to communicate.
2.  Thread that connection as an argument through the whole codebase so
    that RPCs can be dispatched directly from anywhere. By threading the
    connection as an argument instead of always referencing a global
    connection value, we can pass in a different connection for testing
    purposes; otherwise test code would have to spin up a server
    process, which would add an unfortunate amount of non-determinism
    and awkwardness to each test.

The `Rpc_effect` module adds a couple refinements to make this approach
more ergonomic:

3.  Instead of explicitly threading the connection through the whole
    app, `Rpc_effect` uses a dynamic-scoping construct built into Bonsai
    that allows code to pull the connection "out of thin air". This
    reduces parameter-passing noise, at the cost of some extra noise
    anytime something dispatches an RPC.
4.  The module will create connections for you, which reduces the
    typical app setup boilerplate.
5.  `Bonsai_web_test.Handle.create` optionally accepts a set of RPC
    implementations, thus eliminating the boilerplate of setting up a
    connection that works in tests.

# Example 1: Doubling a number

In this example, we will build the UI shown below. It has a button that
sends the current number as an RPC query and sets the number to whatever
number is in the response that comes back.

```{=html}
<iframe data-external="1" src="https://bonsai:8535#double-the-number-rpc">
```
```{=html}
</iframe>
```
The first step is to define an RPC that is shared between the server and
client code.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=protocol-1 -->
```
``` ocaml
let double_rpc =
  Rpc.Rpc.create
    ~name:"double"
    ~version:0
    ~bin_query:[%bin_type_class: int]
    ~bin_response:[%bin_type_class: int]
;;
```

Next, we provide the server's implementation of the RPC.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=implementation-1 -->
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
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=where_to_connect -->
```
``` ocaml
let where_to_connect : Rpc_effect.Where_to_connect.t = Self
```

Finally, we can build the client side of the app.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=client-1 -->
```
``` ocaml
let app =
  let%sub dispatch_double_rpc = Rpc_effect.Rpc.dispatcher double_rpc ~where_to_connect in
  let%sub number, set_number = Bonsai.state ~equal:[%equal: int] 1 in
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

The one line worth highlighting is the one that makes the
RPC-dispatching effect.

`ocaml skip let%sub dispatch_double_rpc = Rpc_effect.Rpc.dispatcher Protocol.double ~where_to_connect in`

This does not by itself dispatch an RPC; rather, it merely produces a
`'query -> 'response Effect.t` function that will do the dispatching.

# Polling State RPC

Bonsai apps often use a library called `polling_state_rpc` whenever they
want to keep up to date with a source of data. The library provides a
similar abstraction to the `Pipe_rpc` or `State_rpc` modules that come
built into `async_rpc_kernel`, but with tradeoffs that are more suitable
for web clients.

Both `Pipe_rpc` and `State_rpc` operate by pushing streams of updates
through the connection; the client can then consume this stream of
events as appropriate. This approach works great if the client is able
to keep up with the stream of events, as is often the case with native
programs. However, web clients are interesting in that they frequently
get backgrounded; if the user switches away from a browser tab, the
Bonsai rendering loop, which relies on the browser's
`requestAnimationFrame` function, slows down to a crawl. If the app uses
`State_rpc`, then when the tab eventually comes back to the foreground,
it might have a large queue of events, which will cause it to freeze
while it catches up.

The `polling_state_rpc` library solves this problem by having the client
say when it is ready for more data by explicitly requesting updates from
the server. This allows the server to give the client a condensed view
of everything that happened since the last time it requested data.

# Example 2: Display the current time

This next example will build a UI that monitors the current time
reported by the server, in order to provide a concrete example of using
the `polling_state_rpc` library.

```{=html}
<iframe data-external="1" src="https://bonsai:8535#poll-the-current-time">
```
```{=html}
</iframe>
```
Ten times a second, the client sends the timezone text as a poll query
to the server, to which it responds with the current time formatted in
the specified zone. Ordinarily the form would be more structured so that
the user can't accidentally send the server an invalid form; however,
this UI does nothing of the sort, in order to illustrate how error
handling might work.

As in the previous example, we will begin by setting up the RPC
definition. The point of interest in this case is that we must provide a
module for the response type that satisfies the `Diffable.S` module type
(from the `diffable` library).

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=protocol-2 -->
```
``` ocaml
module Current_time = struct
  include String
  include Diffable.Atomic.Make (String)
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

Next we provide a server implementation. A call to
`Rpc.Implementation.lift` is common because the implementation you get
from `Polling_state_rpc.implement` requires a `'a * Rpc.Connection.t`
connection state.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=implementation-2 -->
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

Finally, we can write the UI code. There are a couple important ways in
which this example is different from the previous one:

-   Rather than explicitly dispatching the RPC, we merely instantiate
    the poller computation and allow it to send requests as necessary.
    It sends requests whenever the query (the timezone) changes, and
    also at fixed intervals (every tenth of a second).

-   Rather than having to dispatch the RPC to get the response or an
    error, we get a couple constantly available values:
    `last_ok_response` and `last_error`. Each of those values comes
    packaged with the query it was a response to, since it might lag
    slightly behind the most up-to-date query.

The code omits the implementation of `zone_form`, since that is not our
focus in this chapter.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/rpc_examples.ml,part=client-2 -->
```
``` ocaml
let app =
  let%sub zone, zone_view = zone_form in
  let%sub { last_ok_response; last_error; inflight_query = _; refresh = _ } =
    Rpc_effect.Polling_state_rpc.poll
      current_time_rpc
      ~equal_query:[%equal: string]
      ~equal_response:[%equal: Current_time.t]
      ~where_to_connect
      ~every:(Time_ns.Span.of_sec 0.1)
      zone
  in
  let%arr last_ok_response = last_ok_response
  and last_error = last_error
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

# Example 3: Testing a component that sends RPCs

Let us return to the "number doubler" that we built in the first
example. In this section we will build some tests for that component.
Using knowledge from the testing chapter of the guide, our first attempt
might look like the code below.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/test/rpc_examples_test.ml,part=attempt-1 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}]
;;
```

Sadly, it isn't this simple. The first reason is that since RPCs are
dispatched asynchronously, the test must itself be async. Native tests
would accomplish this by adding `open Async` at the top of the test
file, but since we're running in javascript, we have to use a different
library.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/test/rpc_examples_test.ml,part=open-async-js-test -->
```
``` ocaml
open Async_kernel
open Async_js_test
```

Here's what our attempt looks like now. In addition to the above module
imports, we also use `Async_kernel_scheduler.yield_until_no_jobs_remain`
to force any pending side-effects to happen before the test quits. If we
don't include that call, the output still gets printed, but after the
`return ()`, which means the test doesn't typecheck.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/test/rpc_examples_test.ml,part=attempt-2 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect {| "RPC not handled because no connector has been provided." |}];
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
<!-- $MDX file=../../examples/bonsai_guide_code/test/rpc_examples_test.ml,part=attempt-3 -->
```
``` ocaml
let%expect_test "Clicking the button should double the number" =
  let handle =
    Handle.create
      ~rpc_implementations:[ double_implementation ]
      (Result_spec.vdom Fn.id)
      app
  in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 2 </div>
      <button onclick> Double the number </button>
    </div> |}];
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
for testing with RPCs are still in their infancy, so if you have ideas
about how to make writing these tests easier, let us know.
```{=html}
</aside>
```
