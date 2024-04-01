```{=html}
<!-- Here be dragons! Virtual_dom doesn't play well with MDX, so we make a fake library
to run the code examples below.

```ocaml
open! Core
module Bonsai = Bonsai.Cont
open! Bonsai.Let_syntax

module Vdom = struct
  module Attr : sig
    type t

    val on_click : 'a -> t
  end = struct
    type t = unit

    let on_click _ = ()
  end

  module Node : sig
    type t

    val none : t
    val text : 'a -> t
    val button : attrs:'a -> 'b -> t
    val div : t list -> t
  end = struct
    type t = unit

    let none = ()
    let text _ = ()
    let button ~attrs:_ _ = ()
    let div _ = ()
  end
end

```

-->
```
# 04 - State

In the previous chapters, we learned how to build and compose
incremental `Bonsai.t` computations via `let%arr`. But we don't yet have
any `Bonsai.t`s that actually change at runtime. We're missing a key
piece: state!

In this chapter, we'll:

-   Remark on why UI elements should own their state
-   Introduce `Bonsai.state`: a simple getter/setter primitive for state
-   Emphasize that `let%arr`ing on `Bonsai.t`s does not instantiate
    anything
-   Explain `Bonsai.state_machine`, which can better model complex state
    transition logic, and avoid race condition bugs.
-   Emphasize that Bonsai computation graphs have a static shape

## Why Should Functional Programmers Be Okay With State?

Many of the tools that functional programmers use for dealing with state
move it out of their programs into a database, or some small "hazmat"
part of the codebase. These strategies can keep most of your code
relatively pure and easy to test, but don't really work well with UIs.

Most programs produce some single output, updating some global state
through side effects during computation of that output. The view
computed by UIs is actually a structured set of many "leaf" UI elements,
many of which are interactive, and need their own state.

Explicitly aggregating and distributing this state while composing
elements into a UI would be a nightmare: each one would need to manage
potentially dozens or hundreds of states for each transitive
sub-element.

Additionally, if state lived outside of UI elements, any implementation
changes that added/removed/changed internal state would be breaking.

Bonsai's state abstractions provide type-safe wrappers for reading and
changing state, allowing subparts of your UI to own and manage their own
state safely.

## Simple Getter/Setter State

The simplest state tool is `Bonsai.state`, which returns a
`'model Bonsai.t` tracking the current value, and a
`('a -> unit Effect.t) Bonsai.t` "setter [effect](./02-effects.mdx)"
producing function. It takes a default starting value and a
`local_ graph`.

To explore `Bonsai.state`, we'll implement a counter with
increase/decrease buttons. The counter will return a
`Vdom.Node.t Bonsai.t` for the UI, and the current
`count : int Bonsai.t`, which we'll use later.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter -->
```
``` ocaml
let counter (local_ graph) : Vdom.Node.t Bonsai.t * int Bonsai.t =
  let count, set_count = Bonsai.state 0 graph in
  let view =
    let%arr count = count
    and set_count = set_count in
    (* view-construction logic *)
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count - 1)) ]
          [ Vdom.Node.text "-1" ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count + 1)) ]
          [ Vdom.Node.text "+1" ]
      ]
  in
  view, count
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counter_ui">
```
```{=html}
</iframe>
```
## Instantiating State

To create several counters, we can simply call `counter` repeatedly:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=two_counters_correct -->
```
``` ocaml
let two_counters (local_ graph) =
  let counter1, _count1 = counter graph in
  let counter2, _count2 = counter graph in
  let%arr counter1 = counter1
  and counter2 = counter2 in
  Vdom.Node.div [ counter1; counter2 ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#two_counters_correct">
```
```{=html}
</iframe>
```
Critically, instances of state are created when the function is called
with `graph`, **not** when you `let%arr` on the resulting `Bonsai.t`s.
So this:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=two_counters_wrong_1 -->
```
``` ocaml
let two_counters_wrong_1 (local_ graph) =
  let counter, _count = counter graph in
  let%arr counter1 = counter
  and counter2 = counter in
  Vdom.Node.div [ counter1; counter2 ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#two_counters_wrong_1">
```
```{=html}
</iframe>
```
is actually the same as:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=two_counters_wrong_2 -->
```
``` ocaml
let two_counters_wrong_2 (local_ graph) =
  let counter, _count = counter graph in
  let%arr counter = counter in
  Vdom.Node.div [ counter; counter ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#two_counters_wrong_2">
```
```{=html}
</iframe>
```
In both these cases, all 3 counters share the same state, which probably
isn't what you want.

```{=html}
<aside>
```
There's nothing wrong with using the same `val x : Vdom.Node.t` multiple
times, since a `Vdom.Node.t` does not contain any state.
```{=html}
</aside>
```
## State Machine

While `Bonsai.state`'s getter/setter pattern is quite useful, sometimes
your web UI's model more closely resembles a state-machine with
well-defined transitions between states.

There's a tricky bug hidden in our counter: if a user clicks the buttons
twice before Bonsai gets a chance to process the first click, the first
click will be "lost"! This is because the "count" `Bonsai.t` is closed
over by the event handler, so if the button is clicked again before the
new view is computed, the event handler will still have a stale value.

```{=html}
<aside>
```
It's easy to say "clicking on a button that fast isn't particularly
likely", and that may be true, but users can be *very fast* with
keyboard shortcuts!
```{=html}
</aside>
```
There are some tools to deal with stale values at the [Effect.t
level](./02-effects.mdx), but this case is best solved by using
`Bonsai.state_machine0`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=state_machine0 -->
```
``` ocaml
val state_machine0
  :  default_model:'model
  -> apply_action:('action Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> local_ Bonsai.graph
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t
```

```{=html}
<aside>
```
`Bonsai.state_machine0` actually has also some optional
`sexp_of_action`, `sexp_of_model` arguments which you can use to provide
more information to debugging tools.
```{=html}
</aside>
```
Compared to `Bonsai.state`, there are several similarities:

1.  The default model is required.
2.  State is instantiated by passing in a `local_ graph`.
3.  The return value is a tuple of `Bonsai.t`s that provides the current
    state alongside a function which schedules changes to the state.

The main difference is `apply_action`, which is the "state transition"
function of the state machine: "given the current model and an action,
produce a new model." The output also changes: instead of a "setter
effect" function, we get a function that takes an `Action.t` and
produces an `unit Effect.t` to "inject" it into our state machine.

So how would we use `state_machine0` to fix the bug in the counter
application?

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter_state_machine -->
```
``` ocaml
let counter_state_machine (local_ graph) : Vdom.Node.t Bonsai.t * int Bonsai.t =
  let count, inject =
    Bonsai.state_machine0
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model action ->
        match action with
        | `Increment -> model + 1
        | `Decrement -> model - 1)
      graph
  in
  let view =
    let%arr count = count
    and inject = inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text "-1" ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text "+1" ]
      ]
  in
  view, count
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counter_state_machine">
```
```{=html}
</iframe>
```
Now, when a button is clicked multiple times in quick succession,
instead of calling `set_state` multiple times with the same value,
Bonsai will call `inject` multiple times, and they'll be processed by
`apply_action` in order, producing the correct result.

### State Machines with Inputs

What if we wanted to increment / decrement our count by some dynamic
`step : int Bonsai.t`? Our first attempt might look like this:

``` ocaml
# let counter_state_machine ~(step : int Bonsai.t) (local_ graph) =
  let count, inject =
    Bonsai.state_machine0
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model action ->
        let%arr step = step in
        match action with
        | `Increment -> model + step
        | `Decrement -> model - step)
      graph
  in
  let view =
    let%arr count = count
    and inject = inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text "-1" ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text "+1" ]
      ]
  in
  view, count
Lines 6-9, characters 9-37:
Error: This expression has type int Bonsai.t
       but an expression was expected of type int
```

Unfortunately, the compiler doesn't like that. Recall that
`apply_action` for `Bonsai.state_machine0` produces a `'model`, not a
`'model Bonsai.t`. Instead, we'll need some heavier machinery.

`state_machine0` has a "0" at the end to indicate that it takes "0"
additional inputs. There's also a `state_machine1`, which allows
`apply_action` to depend on the current value of a `Bonsai.t`:

``` diff
-val state_machine0
+val state_machine1
   :  default_model:'model
   -> apply_action:
        ('action Apply_action_context.t
+        -> 'input Computation_status.t
         -> 'model
         -> 'action
         -> 'model)
+  -> 'input Bonsai.t
   -> local_ graph
   -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t
```

```{=html}
<aside>
```
If our state machine is [inactive](../how_to/lifecycles.mdx), it cannot
access the current value of `'input`. `Computation_status.t` forces us
to explicitly handle this.
```{=html}
</aside>
```
Let's take `step` as an input and update our implementation to use
`state_machine1`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter_state_machine1 -->
```
``` ocaml
let counter_state_machine1 ~(step : int Bonsai.t) (local_ graph) =
  let count, inject =
    Bonsai.state_machine1
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input model action ->
        match input with
        | Bonsai.Computation_status.Inactive ->
          (* This state machine is inactive, so it can't access the current value of [input].
             Just keep the original model *)
          model
        | Active step ->
          (match action with
           | `Increment -> model + step
           | `Decrement -> model - step))
      step
      graph
  in
  let view =
    let%arr step = step
    and count = count
    and inject = inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text [%string "-%{step#Int}"] ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text [%string "+%{step#Int}"] ]
      ]
  in
  view, count
;;
```

We can even chain our counters together! One counter's `count` can be
used as another counter's `step`, making what can only be described as a
frankencounter:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter_state_machine_chained -->
```
``` ocaml
let counter_state_machine_chained (local_ graph) =
  let counter1, count1 = counter_state_machine1 ~step:(Bonsai.return 1) graph in
  let counter2, count2 = counter_state_machine1 ~step:count1 graph in
  let counter3, _ = counter_state_machine1 ~step:count2 graph in
  let%arr counter1 = counter1
  and counter2 = counter2
  and counter3 = counter3 in
  Vdom.Node.div [ counter1; counter2; counter3 ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counter_state_machine_chained">
```
```{=html}
</iframe>
```
There is no `state_machine2` (or n), because multiple inputs could be
packaged together as a single `Bonsai.t`, and destructured inside
`apply_action`.

### State Machines can Schedule Effects

The `apply_action` function also receives an `Apply_action_context.t`,
which can

1.  schedule arbitrary `unit Effect.t`s via
    `Apply_action_context.schedule_event`
2.  dispatch other actions into itself with
    `Apply_action_context.inject`

This is necessary for any state machine that wants to send messages to
itself, e.g. when dealing with timeouts.

It's also useful for stitching together components that talk to each
other.

```{=html}
```
```{=html}
```
## Other State Primitives

Bonsai has some other tools for state, such as `Bonsai.state_opt`,
`Bonsai.toggle`, and `Bonsai.actor`. You should read the [API
Docs](https://github.com/janestreet/bonsai/blob/master/src/bonsai.mli)
to learn more.

All Bonsai state primitives also take an optional `reset` argument,
which allows you to control what happens when [state is
reset](../how_to/resetting_state.mdx).

```{=html}
```
Let's continue to [Bonsai Guide Part 5: Control
Flow](./05-control_flow.mdx).

## The Underlying Machinery

### `local_ graph` is a Graph Builder

`local graph : Bonsai.graph` is used by Bonsai to build a
[static](../advanced/why_no_bind.mdx) computation graph. Most nodes in
the graph come from `let%arr` calls, but many "leaves" of the graph are
"state" nodes. At startup, Bonsai aggregates the entire state of your
app.

The [`local_`
mode](https://blog.janestreet.com/oxidizing-ocaml-locality/) prevents
`graph` from being closed over / stashed away, so the compiler makes it
impossible to change the computation graph from any runtime code.

Bonsai analyzes the entire computation at startup time and performs
optimizations to make apps faster!

```{=html}
<aside>
```
`let%arr` actually uses `graph` so that we can share the work done by
one `Bonsai.t` in multiple places! But it wouldn't be ergonomic to pass
it in every time, so Bonsai's internals cheat and access it implicitly.
```{=html}
</aside>
```
### Most Code Runs Once!

When writing Bonsai code, you're actually doing 2 different things:

-   Defining the static computation graph; i.e. *what* is computed, and
    *which* inputs it has.
-   Dictating runtime behavior of the web app; i.e. *how* it is
    computed.

Only the contents of `let%arr` blocks (everything after the `in`),
`apply_action` state transition functions, and functions used to
construct [effects](./02-effects.mdx) are "runtime" code. Everything
else only runs *exactly once* at app startup to construct the
computation graph, before it gets compiled to a `Vdom.Node.t Incr.t`.
