# 05 - Effect

As we saw back in the [chapter about state](./03-state.md), values with
type `unit Vdom.Effect.t` are used to schedule updates to stateful
components. However, the `Effect.t` type can also be used to perform
arbitrary side-effectful actions that return values. Most commonly,
these side effects involve calling RPCs.

A `'a Effect.t` represents a side effect which, when performed, produces
a value of type `'a`.

There's a lot of overlap between `'a Effect.t` and `'a Deferred.t`:

1.  Both are (likely) performing side effects (like calling RPCs)
2.  They produce values of type `'a` when completed
3.  This result can be computed at some point in the future

So it's important to note one major difference between `'a Effect.t` and
`'a Deferred.t`: when bound (via `let%bind`) multiple times, a
`Deferred` will execute its side effect exactly once, but an `Effect`
will side effect as many times as it is `bound`.

This difference exists for both theoretical and practical purposes.

On the theoretical side, `Deferred.t`, at its core, represents a value
that will be computed at some point in the future (and may perform side
effects in order to calculate that value), while `Effect.t` is a
first-class representation of the side effect itself, which happens to
produce a value.

On the practical side, `Deferred.t` just doesn't mesh with the
incremental computational model that Bonsai provides. In particular, a
value of type `'a Deferred.t Value.t` is quite hard to use correctly, as
Bonsai has no way of knowing that the value contained inside is a
Deferred, and it won't re-compute when the deferred is completed.

```{=html}
<!--
Also, unit tests for Bonsai\_web applications require compiling to JavaScript,
and our JavaScript expect-test library is not capable of running Async tests,
so `Effect.t` is used with synchronous functions to test apps that will use
asyncronous functions in production.
-->
```
# Making an Effect

The main use-case for Effect is for exposing RPCs to the Bonsai
application, so for the rest of this document, we're going to be
interacting with a function that has this type signature, which we'll
pretend is an RPC:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val uppercase : string -> string Deferred.t
```

Turning `uppercase` into a function that returns an `Effect` is easy
with `Bonsai_web.Effect.of_deferred_fun`

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val of_deferred_fun : ('query -> 'response Deferred.t) -> 'query -> 'response t
```

```{=html}
<aside>
```
`of_deferred_fun` is defined inside `Bonsai_web` instead of just
`Bonsai` because `Bonsai` doesn't depend on `Async`. All the rest of the
Effect functions are in `Bonsai.Effect` (but re-exported for
`Bonsai_web.Effect`).
```{=html}
</aside>
```
Using `Bonsai_web.of_deferred_fun`, we can make a new function that
returns an `Effect.t` instead of `Deferred.t`

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_examples.ml,part=uppercase_e -->
```
``` ocaml
let uppercase_e : string -> string Effect.t = Bonsai_web.Effect.of_deferred_fun uppercase
```

# Using Effects

By converting a deferred-returning function to return an effect, we can
more easily compose it with other Bonsai APIs, like event handlers.

In the following example, we have a textbox, a button, and a "results"
display. We want to use the `uppercase_e` event-returning function from
above to compute the uppercased value of the contents of the textbox
when the button is clicked.

The first implementation looks like this.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_examples.ml,part=uppercase_rpc_sender -->
```
``` ocaml
module Request_state = struct
  type t =
    | Empty
    | Pending
    | Filled of string
  [@@deriving sexp, equal]

  let to_string = function
    | Empty -> "<no request sent>"
    | Pending -> "pending..."
    | Filled s -> s
  ;;
end

let uppercase_rpc_sender =
  let%sub textbox = Forms.Elements.Textbox.string () in
  let%sub result_state = Bonsai.state (module Request_state) ~default_model:Empty in
  let%arr textbox = textbox
  and result_state, set_result = result_state in
  let on_submit (contents : string) : unit Effect.t =
    let%bind.Effect s = uppercase_e contents in
    set_result (Filled s)
  in
  let form_view =
    textbox
    |> Forms.label "text to capitalize"
    |> Forms.view_as_vdom ~on_submit:(Forms.Submit.create ~f:on_submit ())
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.style (Css_gen.display `Inline_grid) ]
    [ form_view; Vdom.Node.text (Request_state.to_string result_state) ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#uppercase_rpc_sender">
```
```{=html}
</iframe>
```
```{=html}
<aside>
```
Please note that the "Pending" state is not used (yet!)
```{=html}
</aside>
```
Let's zoom in on the `on_submit` handler:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let on_submit (contents : string) : unit Effect.t =
  let%bind.Effect s = uppercase_e contents in
  set_result (Filled s)
```

By calling the `uppercase_e` function, a `string Effect.t` is returned.
Binding on that value gives us (at some point in the future) the result
of the operation, which we immediately pass through to update the state
of our component.

But as mentioned above, the "Pending" state was never used. We can
implement that by adding another bind to the effect, setting "Pending"
immediately.

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let on_submit (contents : string) : unit Vdom.Effect.t =
  let open Bonsai.Effect.Let_syntax in
  let%bind () = set_result Pending in
  let%bind s = uppercase_e contents in
  set_result (Filled s)
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#uppercase_rpc_sender_bind">
```
```{=html}
</iframe>
```
Next, read the [chapter on testing](../blogs/testing.md).
