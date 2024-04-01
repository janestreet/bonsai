# Resetting State

We can reset all state created within some block of code by wrapping it
in `Bonsai.with_model_resetter`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=resettable_counters -->
```
``` ocaml
let two_counters (local_ graph) =
  let%arr counter1 = State_examples.counter_ui graph
  and counter2 = State_examples.counter_ui graph in
  Vdom.Node.div
    ~attrs:[ [%css {|border: 1px solid black; padding: 4px|}] ]
    [ counter1; counter2 ]
;;

let reset_ui (local_ graph) ~f =
  let view, reset = Bonsai.with_model_resetter ~f graph in
  let%arr view = view
  and reset = reset in
  Vdom.Node.div
    [ view
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> reset) ]
        [ Vdom.Node.text "Reset" ]
    ]
;;

let resettable_counters = reset_ui ~f:two_counters
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#resettable_counters">
```
```{=html}
</iframe>
```
There's also a `Bonsai.with_model_resetter'`, which allows blocks of
code to reset their own state:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=resettable_counters_from_inside -->
```
``` ocaml
let resettable_counters_from_inside (local_ graph) =
  Bonsai.with_model_resetter'
    ~f:(fun ~reset (local_ graph) ->
      let%arr counter1 = State_examples.counter_ui graph
      and counter2 = State_examples.counter_ui graph
      and reset = reset in
      Vdom.Node.div
        ~attrs:[ [%css {|border: 1px solid black; padding: 4px|}] ]
        [ counter1
        ; Vdom.Node.button
            ~attrs:[ Vdom.Attr.on_click (fun _ -> reset) ]
            [ Vdom.Node.text "Reset" ]
        ; counter2
        ])
    graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#resettable_counters_from_inside">
```
```{=html}
</iframe>
```
## Advanced: Customizing Reset Behavior

By default, when the effect from a `with_model_resetter` is scheduled,
it resets all states instantiated inside it to their `default_model`s.
In most cases, this is the desired behaviour. However, there are a
couple of niche situations where you might want to customize the
behaviour by providing a custom `reset` function on state creation.

### Ignoring Resets

One use case of Bonsai state is tracking some non-Bonsai value. For
instance, let's say we want to use the following API for getting the
status of an RPC connection:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=connection_type -->
```
``` ocaml
module Connection : sig
  (* Some generic connection type that doesn't know of Bonsai *)
  type t

  (* Connect to some resource *)
  val create : uri:string -> t

  (* Notify subscribers whenever the connection status changes *)
  val on_status_change
    :  t
    -> ([ `Connected | `Connecting | `Disconnected ] -> unit Effect.t)
    -> unit Effect.t
end
```

We could make its status available as a `Bonsai.t` by creating some
state, and updating it via the `on_status_change` subscription:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=connection_status -->
```
``` ocaml
let connection_status (local_ graph) conn
  : [ `Connected | `Connecting | `Disconnected ] Bonsai.t
  =
  let status, set_status = Bonsai.state `Disconnected graph in
  let register_status_change =
    let%arr set_status = set_status in
    Connection.on_status_change conn set_status
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:register_status_change graph in
  status
;;
```

We could then use this `status Bonsai.t` as an input to a UI that lets
the user know if they are connected:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=connection_status_ui -->
```
``` ocaml
let conn = Connection.create ~uri:"https://google.com"

let connection_status_ui (local_ graph) =
  let connection_status =
    match%sub connection_status graph conn with
    | `Connected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connected" ])
    | `Connecting -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Connecting" ])
    | `Disconnected -> Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Disconnected" ])
  in
  let%arr connection_status = connection_status in
  Vdom.Node.div [ connection_status ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#connection_status_ui">
```
```{=html}
</iframe>
```
This works fine, but things get a bit dicey if our connection status
function is included transitively in a call to `with_model_resetter`.
For example:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=resettable_connection_and_counters -->
```
``` ocaml
let connection_and_counters (local_ graph) =
  let%arr connection_status_ui = connection_status_ui graph
  and counters = two_counters graph in
  Vdom.Node.div [ connection_status_ui; counters ]
;;

let resettable_ui = reset_ui ~f:connection_and_counters
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#resettable_connection_and_counters">
```
```{=html}
</iframe>
```
The `state` we use to track `connection_status` isn't really *state*: it
should persist across model resets. But if you click the reset, in
addition to resetting the counters, the connection status switches to
"Disconnected" until the next time the connection status *actually*
changes.

We can exclude state from being reset by providing a custom `reset`
function:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=connection_status_reset -->
```
``` ocaml
let connection_status (local_ graph) conn
  : [ `Connected | `Connecting | `Disconnected ] Bonsai.t
  =
  let status, set_status =
    Bonsai.state ~reset:(fun model_when_reset -> model_when_reset) `Disconnected graph
  in
  let register_status_change =
    let%arr set_status = set_status in
    Connection.on_status_change conn set_status
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:register_status_change graph in
  status
;;
```

And now we observe the correct reset behaviour for both the counter and
connection status UIs:

```{=html}
<iframe data-external="1" src="https://bonsai:8535#proper_reset_ui_with_connection_status">
```
```{=html}
</iframe>
```
```{=html}
<aside>
```
Bonsai has some optimizations that apply when a state machine's current
model is equal to its default model. Customizing `reset` to return
something other than the default model can impact these optimizations,
so you should only do it when necessary.
```{=html}
</aside>
```
### Performing Cleanup

In contrast, some Bonsai state machines may serve as the source of truth
for data, and work to make the *outside world* be consistent with that
data. For example, a trading UI might have a component that keeps track
of open/filled orders, and issues requests to an exchange to enact those
orders.

An over-simplified interface for an exchange might look like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=exchange_type -->
```
``` ocaml
module Exchange : sig
  module Order_id = Int

  (* A connection to an exchange *)
  type t
  type event = Fill of Order_id.t

  val create : unit -> t

  (* Sends an order to the exchange *)
  val send_order : t -> Order_id.t -> unit Effect.t

  (* Cancels an open order on the exchange *)
  val cancel_order : t -> Order_id.t -> unit Effect.t

  (* Subscribe to notifications of which orders have been filled *)
  val subscribe : t -> (event -> unit Effect.t) -> unit Effect.t
end
```

And we could write a state machine that lets users create and track
their open and filled orders:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=order_manager_definition -->
```
``` ocaml
module Model = struct
  type t =
    { open_orders : Order_id.Set.t
    ; filled_orders : Order_id.t list
    ; next_order_id : int
    }

  let empty = { open_orders = Order_id.Set.empty; filled_orders = []; next_order_id = 0 }
end

module Action = struct
  type t =
    | Create_ui_order
    | Received_fill of Exchange.Order_id.t
end

let order_manager (exchange : Exchange.t) (local_ graph) =
  let model, inject_action =
    Bonsai.state_machine0
      ~default_model:Model.empty
      ~apply_action:
        (fun
          context { open_orders; next_order_id; filled_orders } (action : Action.t) ->
          match action with
          | Create_ui_order ->
            let this_order_id = next_order_id in
            let open_orders = Set.add open_orders this_order_id in
            Apply_action_context.schedule_event
              context
              (Exchange.send_order exchange this_order_id);
            { open_orders; next_order_id = this_order_id + 1; filled_orders }
          | Received_fill order_id ->
            let filled_orders = filled_orders @ [ order_id ] in
            let open_orders = Set.remove open_orders order_id in
            { open_orders; next_order_id; filled_orders })
      graph
  in
  let subscribe_to_exchange =
    let%arr inject_action = inject_action in
    Exchange.subscribe exchange (fun (Fill order_id) ->
      inject_action (Received_fill order_id))
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:subscribe_to_exchange graph in
  let inject_new_order =
    let%arr inject_action = inject_action in
    inject_action Create_ui_order
  in
  let open_orders =
    let%arr { open_orders; _ } = model in
    open_orders
  in
  let filled_orders =
    let%arr { filled_orders; _ } = model in
    filled_orders
  in
  open_orders, filled_orders, inject_new_order
;;
```

Our `order_manager` keeps track of open and filled orders that we get
from the exchange. It also provides an action we can inject to open an
order. When it receives that action, it updates its state and also sends
an order to the exchange, keeping the state of the outside world in sync
with itself.

We can incorporate our `order_manager` into our trading UI:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=trading_ui -->
```
``` ocaml
let trading_ui exchange (local_ graph) =
  let open_orders, filled_orders, inject_new_order = order_manager exchange graph in
  let%arr open_orders = open_orders
  and filled_orders = filled_orders
  and inject_new_order = inject_new_order in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Open orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (open_orders : Order_id.Set.t)]
    ; Vdom.Node.text [%string "Filled orders:"]
    ; Vdom.Node.sexp_for_debugging [%sexp (filled_orders : Order_id.t list)]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> inject_new_order) ]
        [ Vdom.Node.text "New order" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#trading_ui">
```
```{=html}
</iframe>
```
However, as in the previous example, something weird starts to happen if
our trading UI happens to be nested in a model resetter. In the demo
below, try resetting the computation when there are open orders.

```{=html}
<iframe data-external="1" src="https://bonsai:8535#trading_ui_reset">
```
```{=html}
</iframe>
```
As you may have noticed, we can still receive fills for orders that were
opened before resetting our model, even though resetting the model was
supposed to restore our state to `Model.default`, which has no open
orders.

Resetting the model doesn't automatically cancel our open orders on the
exchange: we need to do this ourselves. We'll change the use of
`Bonsai.state_machine0`, providing a `reset` implementation that uses
the `Apply_action_context.t` parameter:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_reset_examples.ml,part=order_manager_with_reset -->
```
``` ocaml
  let model, inject_action =
    Bonsai.state_machine0
      ~default_model:Model.empty
      ~apply_action:
        (fun
          context { open_orders; next_order_id; filled_orders } (action : Action.t) ->
          match action with
          | Create_ui_order ->
            let this_order_id = next_order_id in
            let open_orders = Set.add open_orders this_order_id in
            Apply_action_context.schedule_event
              context
              (Exchange.send_order exchange this_order_id);
            { open_orders; next_order_id = this_order_id + 1; filled_orders }
          | Received_fill order_id ->
            let filled_orders = filled_orders @ [ order_id ] in
            let open_orders = Set.remove open_orders order_id in
            { open_orders; next_order_id; filled_orders })
      ~reset:(fun context (model : Model.t) ->
        Set.iter model.open_orders ~f:(fun order_id ->
          Apply_action_context.schedule_event
            context
            (Exchange.cancel_order exchange order_id));
        Model.empty)
      graph
  in
```

With this new reset behaviour, orders don't persist between model
resets.

```{=html}
<iframe data-external="1" src="https://bonsai:8535#proper_trading_ui_reset">
```
```{=html}
</iframe>
```
```{=html}
<aside>
```
Using model-reset events to cancel orders on an exchange is a pretty
contrived example. In practice, there are better ways to build this UI,
since your web client shouldn't be the source of truth for your orders.
```{=html}
</aside>
```
