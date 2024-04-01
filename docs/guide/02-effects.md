# 02 - Effects

In the previous chapter, we built a `clicky` button that used
`Effect.alert` and an `on_click` listener attr to show browser alerts
whenever a user clicks a button.

This chapter explains the `Effect.t` type.

```{=html}
<aside>
```
`Effect.t` is an alias for `Ui_effect.t`, which you might see in other
libraries or merlin. The convention is to use `Effect.t`.
```{=html}
</aside>
```
## What Is `Effect.t`?

A `'a Effect.t` encapsulates some side effect, which may execute
asynchronously and eventually produce a value of type `'a`. Common
effects that you'll likely use include:

-   Setting/updating [state](./04-state.mdx)
-   Focusing [form elements](../how_to/forms.mdx)
-   Invoking [RPCs](../how_to/rpcs.mdx)

At first glance, `'a Effect.t` looks very similar to [Async's
`'a Deferred.t`](https://dev.realworldocaml.org/concurrent-programming.html).
Both represent potentially asynchronous side effects, and both are
monadic, which means that they have a `bind` operator which evaluates
the side-effects in sequence.

The main difference between the two is that `Effect` is "pure", meaning
that:

1.  "making" an `Effect.t` doesn't trigger the side-effect. The actual
    side-effect isn't performed until the effect is scheduled.
2.  scheduling an `Effect.t` multiple times will trigger the side-effect
    multiple times. Contrast this with `Deferred.t`, where you can
    `bind` on the same deferred twice and it'll only run once.

Here's a demonstration:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_examples.ml,part=clickies -->
```
``` ocaml
let clickies : Vdom.Node.t =
  (* This won't run until scheduled...
     But it will run every time it is scheduled! *)
  let greet_effect = Effect.alert "hello there!" in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun (_evt : mouse_event) -> greet_effect) ]
        [ Vdom.Node.text "click me!" ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun (_evt : mouse_event) -> greet_effect) ]
        [ Vdom.Node.text "or me!" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clickies">
```
```{=html}
</iframe>
```
## How to Get `Effect.t`s

Many Bonsai tools and libraries will return some `'a Effect.t`s. For
example:

-   Bonsai's [state primitives](./04-state.mdx) return `Effect.t`s to
    modify the state.
-   [Rpc_effect](../how_to/rpcs.mdx) return a `'response Effect.t` for
    dispatching an RPC call.
-   A modal library might return `Effect.t`s that open/close the modal.
-   The Effect module contains some [commonly used effects for browser
    APIs](../how_to/effects_for_browser_apis.mdx)

You can also wrap arbitrary side-effectful OCaml functions in
`Effect.t`s:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Effect.of_sync_fun : ('query -> 'result) -> 'query -> 'result Effect.t
val Effect.of_thunk : (unit -> 'result) -> 'result Effect.t

val Effect.of_deferred_fun : ('query -> 'response Deferred.t) -> 'query -> 'response Effect.t
val Effect.of_deferred_thunk : (unit -> 'response Deferred.t) -> 'response Effect.t
```

These are most useful for authors of reusable components / libraries;
you generally shouldn't use them in app code.

## How to Compose `Effect.t`s

The `Effect.t` type is a
[monad](https://builtin.com/software-engineering-perspectives/monads),
which means we can sequence `Effect.t`s with `let%bind`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_examples.ml,part=bind_chain -->
```
``` ocaml
let chain_some_effects
      (a : int Effect.t)
      (b : int -> bool Effect.t)
      (c : unit Effect.t)
      (d : unit Effect.t)
  : unit Effect.t
  =
  let%bind.Effect a_return = a in
  (* Sometimes we don't care about the effect's return value;
     we just want to execute it. *)
  let%bind.Effect (_ : bool) = b a_return in
  let%bind.Effect () = c in
  d
;;
```

If you don't care about passing anything between effects, and just want
to run them in sequence, there are some utils implemented via `bind`:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Effect.all_unit : unit Ui_effect.t list -> unit Ui_effect.t
val Effect.all : 'a Ui_effect.t list -> 'a list Ui_effect.t
```

There's also an `Effect.Many`, which takes a list of `unit Effect.t`s,
dispatches them in parallel, and does not wait for any of them to
complete.

## How to Schedule `Effect.t`s

Most commonly, effects are scheduled in response to user interactions
with the web UI, using `Vdom.Attr.*` event handlers.

But you can also schedule `Effect.t`s:

-   When [your code becomes active / inactive](../how_to/lifecycles.mdx)
-   When an [incremental value
    changes](../how_to/edge_triggered_effects.mdx)
-   At a [particular time](../how_to/time.mdx)
