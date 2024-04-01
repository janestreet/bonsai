How Bonsai Works

This article talks about how Bonsai works. It is probably most useful to
Bonsai maintainers, although it can provide useful context for power
users as well. We'll cover:

-   How Bonsai's internals work
-   How the `local_ graph` API works

```{=html}
<aside>
```
Most of these details are very implementation-specific, and might become
outdated with changes to Bonsai's internals.
```{=html}
</aside>
```
# `Computation.t` and `Value.t`

One of Bonsai's invariants is that the computation graph is a static
DAG. This offers [a lot of benefits](./why_no_bind.mdx).

Bonsai's internal representation of this graph is based around 2 types.

`Value.t` is effectively a wrapper around Incremental's `Incr.t`, and
ultimately gets compiled down to `Incr.t`s.

`Computation.t` represents the "structure" of the computation graph.
It's implemented as a variant of types such as:

-   `Leaf0` and `Leaf1`: structure around state nodes
-   `Store` and `Fetch`: powers Bonsai's `Dynamic_scope` tools
-   `Assoc`: allows creating a dynamic number of copies of some internal
    `Computation.t`, each with their own state / lifecycle.
-   `Switch`: allows selecting one of several `Computation.t`s to
    evaluate.
-   `Path`: returns the path to a new node in the computation graph,
    which is useful as a unique ID.
-   `Lifecycle`: allows running `on_activate` / `on_deactivate` /
    `after_display` lifecycle events
-   `Return`: a very important one: just return an incremental
    `Value.t`.
-   `Sub`: possibly the most important one: allows a computation to use
    the output of **any other** computation in the graph. This is what
    makes the computation graph a DAG, and not a tree.

## How do `Computation.t` and `Value.t` interact?

Many of the variants of `Computation.t` are records, that store some
`Value.t`. For example, `Switch` stores an `int Value.t` representing
which of the `Computation.t`s is currently active. And `Assoc` stores an
input `Map.t`. Of particular importance, `Return` stores some arbitrary
`Value.t`.

## How was the graph constructed before?

Up until late 2023, Bonsai's public-facing API directly constructed a
`Computation.t` graph, which provides structure for `Value.t`
incremental calculations. Let's go over the key parts.

### sub

`val sub: 'a Computation.t -> f:('a Value.t -> 'b Computation.t) -> 'b Computation.t`.
This was commonly used via the `let%sub` syntax. It:

-   Creates a new "named" `Value.t`, which essentially reads
    `'a Computation.t`.
-   Applies the provided `f` function to that value, generating a new
    `'b Computation.t`.
-   Creates a new `Sub` node for the graph, which contains these 2
    `Computation.t`s.

Essentially, this allows us to compute a `'a Value.t`, use it in 2
separate computations, and then `map` them together.

If we only had a tree, we would have to clone that `'a Value.t` for each
of the 2 subcomputations. We would have to redo all calculations twice,
and we couldn't share state.

But with `sub`, we essentially create a symlink / reference / pointer to
the shared `'a Value.t`, so we can use it in multiple places without
issue. This is why `Sub` is so important.

### arr

`val arr : 'a Value.t -> f:('a -> 'b) -> 'b Computation.t`. There are
also `arr2`, `arr3` versions, which combine several `Value.t`s into one
`Computation.t`.

This was commonly used via the `let%arr` syntax. All `arr` does is call
`Value.map`, and wrap the result in a `Return` node.

The entire point of `arr` is to force you to use `sub`, so that our new
`Value.t` is defined in terms of references/symlinks to any shared work,
and we don't accidentially clone that work / lose shared state.

### And the rest?

We've talked about the `Sub` node via `sub`, and how new `Value.t`s get
created (and then wrapped in `Return`) via `arr`. What about the other
`Computation.t`s?

This is where all the `Bonsai.state_machine`, `Bonsai.Dynamic_scope.*`,
`Bonsai.assoc`, etc primitives come in; they create `Leaf0`,
`Fetch`/`Store`, `Assoc`, and other nodes. Then, using `sub` on those
nodes adds them into the graph.

## Ok, but how do we get an `Incr.t` from all of this?

Before Bonsai boots up your web UI, it performs a compilation pass.
First, it recursively "evaluates" your computation graph, resulting in a
single `Computation.info` record.

```{=html}
<aside>
```
We actually return a `Computation.packed_info`, which uses GADTs to be
able to store a `Computation.info` without specifying the `'model`,
`'input`, `'action`, etc types.
```{=html}
</aside>
```
### What is Computation.info

For every `Computation.t`, we recursively create this
`Computation.info`, which includes:

-   `'model Meta.Model.t`, which contains the default value / type id
    structure for the `Computation`.
    -   For many `Computation.t`s, such as `Path`, `Lifecycle`, and
        `Return`, this is just `Meta.Model.unit`.
    -   For the "state" nodes `Leaf0` and `Leaf1`, this is the
        user-provided model metadata.
    -   For nodes defined in terms of other `Computation.t`s, such as
        `Sub`, `Assoc`, `Switch`, and `Store`, we recursively combine
        the `Meta.Model.t` of subcomputations.
-   `'input Meta.Input.t`, which contains a type id for inputs to the
    `Computation.t`. For `Leaf1`, which is a state machine with input,
    this is the user-provided input metadata. For recursive
    `Computation.t`s, this combines inputs of subcomputations.
    Otherwise, it's `Meta.Input.unit`.
-   `'action Action.id`, which is a type id for a state machine's
    `Action.t`, is assembled similarly.
-   As is `reset`, which is a `'model -> 'model` function used to
    implement `with_model_resetter`.
-   And so is `apply_action`, which users provide when creating state
    machines, and is the state machine transition, with the added
    ability to dispatch events.

### `run` and `Snapshot.t`

The final, and most interesting part of `Computation.info` is `run`,
which takes:

-   An "environment", which contains all "named" `Value.t`s memoized via
    `sub`, andthe available dynamically scoped variables.
-   A `Time_source.t`
-   The current computation path
-   An `Incr.t` with the current state
-   An `inject` function, which creates `Effect.t`s that apply
    `'action`s

and outputs a `Snapshot.t`. which represents the state of the
computation, and all its subcomputations.

`Snapshot.t` packages an `Input.t`, which is all `Incr.t` (and empty
`unit`) inputs to the computation and its subcomputations, some
lifecycle state, and a result, which is an `Incr.t` with the dynamic
value produced by the computation.

So essentially, `run` is the "logic" of the `Computation.t`. Let's
discuss a few examples:

For `Return`, `run` creates a trivial snapshot with no input and no
lifecycle. It calls `Value.eval` on its internal `Value.t`, and uses
that for the `Snapshot.t`'s result. `Value.eval` is provided the
"environment", so that `Value.t`s built in terms of "named"
symlink/reference/pointer `Value.t`s can be evaluated.

For `Leaf0`, `run` is similar, except that it returns the current state
`Incr.t` and the `inject` function for `result`. `Leaf1` is almost the
same, except that it uses the `input : 'input Value.t` field of its
`Computation.t` node for the `Snapshot.t`'s input.

`Store`'s `run` is probably the simplest out of computations with
subcomputations; it adds whatever dynamically scoped value it is storing
into the environment, then calls the inner computation's `run` with this
new environment.

For the rest, read the implementation in
[bonsai/src/eval](https://github.com/janestreet/bonsai/blob/master/src/eval.ml).

### So where do models + actions live?

The top-level `Computation.info` for our whole web UI now contains a
`'model Meta.Model.t`, which represents the structure + default values
of the state of everything. We can use that to create a
`'model Incr.Var.t`, which will be a single variable tracking the state
of our whole web UI!

It's shaped roughly like the `Computation.t` graph itself: every `Sub`
is a tuple of the `'model` of its `from` and the `'model` of its `into`;
assocs are `Map.t`s of the inner's `'model`, and leaf `'model`s are what
you'd expect.

There are similar tree-like structures storing the `apply_action` and
`reset` functions for everything. And the "actual" representation of an
action is a path in that tree.

You can think of `gather` as a pass up the tree, which collects the
structure of everything and assembles these whole-app state/action/etc
trees, and a second pass down the tree, which provides `Incr.t`
accessors for recursive sub-parts of those trees.

### And then what?

The private `bonsai.driver` library uses `eval` to construct this
`Computation.info`, instantiates the environment, default model,
`Effect.t` queue, and some other things, calls `run` on the top-level
computation, extracts the `result Incr.t` and `input Incr.t` from the
resulting snapshot, and puts these all together in a `Bonsai_driver.t`
type that provides an API for running the Bonsai computation.

This driver is mostly used for testing.

### Testing??? I want a website!

`Bonsai_web.Start` pretty much does the same thing, except that it does
some web specific stuff, and expects a `Vdom.Node.t Computation.t`. Once
it has a `Vdom.Node.t Incr.t`, it uses `virtual_dom` to generate DOM
from the vdom / attach it to some element, and then run `virtual_dom`'s
diff-and-patch algorithm when the `Incr.t` changes.

`Bonsai_web.Start` actually mostly does this through
`Incr_dom.Start_app`.

# The `local_ graph` API

In late 2023, we released a new implementation of Bonsai's API, called
`Cont`. That's because it is implemented in the continuation-passing
style.

As covered above, in the previous API, calls to `sub`, `arr`, and
`Bonsai.*` primitives directly created `Computation.t` nodes (and the
underlying `Value.t`s).

The new API doesn't actually change the internals! As before,
`Computation.t` is the static structure of the computation graph, and
`Value.t` is a wrapper around `Incr.t`, adding support for
"reference/symlink" values, which enable `sub` and work-sharing, and a
bunch of other stuff.

In the new API, we have one `Bonsai.t` type, which is actually just
`Value.t`. Instead of `Computation.t`s, we pass around an
abstractly-typed `Bonsai.graph` value. This `graph` value is used to
construct Bonsai's computation graph, as we'll discuss in a bit.

## Why do we need `graph`, and what is `local_`?

There are two "phases" for Bonsai apps:

1.  "`Computation.t` building", where we build up the static computation
    graph.
2.  "Runtime", which starts when `Bonsai_web.Start.start` is run. At
    this point, the Computation.t templates are instantiated and flush
    with data. Runtime code is all in functions provided to
    `Bonsai.map`; `let%arr` hides this somewhat, but the pure OCaml
    "contents" of `let%arr` calls are actually de-sugared to anonymous
    functions.

Once "runtime" starts, you can no longer do any `Computation.t`
building; there aren't any `Computation.t`s left to build! It's all just
one big `Incr.t`! And in fact, Bonsai's type signatures force separation
of "computation building" code and "runtime" code, even though all this
code lives together in the same files and functions.

But with a new API, where there's just one `Bonsai.t` type, how do we
enforce this separation? What's stopping us from referring to `graph` in
runtime code, inside of some `let%arr`, and blowing up our application?

The new [local\_
mode](https://blog.janestreet.com/oxidizing-ocaml-locality/) gives us a
new way to have the type-checker guarantee the separation of phases! To
dramatically oversimplify `local_`: when a function annotates an
argument with `local_`, it's promising not to close over it, partially
apply it to any variables, put it in a ref, or otherwise stash it away.

Because all our runtime code is put in closures, use of `local_` means
that we just can't use `graph`! And so, we get a "phase witness"
property in `graph` through `local_`.

As a result, Bonsai can enforce the same invariants as before, but
provide a simpler API and provide even more power and flexibility to UI
authors!

But what's going on under the surface? And what is `graph`? And why can
the new Bonsai API use `Bonsai.map`?

## How `local_ graph` Works

### Background on `Sub`

Recall that the `sub` is the memoization-like pattern that powers
work-sharing for incrementality: we can store a reusable value with a
unique name (via Type_equal.id), which downstream incremental `Value.t`s
can reference (and reuse!) instead of repeating work. The "Sub"
`Computation.t` node has 3 fields:

-   `from` is the computation being memoized
-   `via` is the `Type_equal.id` identifier
-   `into` is the downstream computation that uses the value of `from`.

Let's say we want to use the output of `comp1` and `comp2` (both
`Computation.t`s) in another computation called
`thing_we_want_to_compute`. This is what the corresponding computation
graph looks like:

```{=html}
<!-- $MDX skip -->
```
    Sub { from=comp1; via=<name1>; into =
        Sub { from=comp2; via=<name2>; into =
            ... into = thing_we_want_to_compute ...
        }
    }

### What is `graph`, and how is `Cont` implemented?

The new `Cont` implementation has a very primitive API (not exposed to
end-users), which simulates algebraic effects:

-   `perform` takes a `local_ graph -> Computation.t`, evaluates it, and
    adds it into the computation graph. It returns a `Value.t` memoized
    alias to this new computation.
-   `handle` places a `Value.t` in the context of a fresh computation
    graph. It then wraps the result in a `Computation.t`. This is
    generally used to construct a portion of the computation graph in
    isolation. The subgraph is then added to the main graph via
    `perform`.

`graph` is the mechanism for composing / combining `Computation.t`s via
`Sub`. It is implemented as a ref holding a
`Computation.t -> Computation.t` function. `graph` starts as `Fn.id`. At
each invocation of `perform`, we overwrite it to still return its input,
but nested in a new layer of `Sub`.

```{=html}
<aside>
```
This approach is very similar to how [Difference
lists](https://en.wikipedia.org/wiki/Difference_list) are typically
implemented, except instead of operating over lists, we operate over
\[Computation.t\]s.
```{=html}
</aside>
```
Let's look at how this works in practice. The top part is the code, and
the bottom is the value of `graph` after every line:

```{=html}
<!-- $MDX skip -->
```
    1.let computation graph =
    2.  let model1, _inject = Bonsai.state 5 graph in
    3.  let model2, _inject = Bonsai.state 6 graph in
    4.  model2

    ---

    1. graph = { f = fun r -> r }
    2. graph = { f = fun r -> Sub { from=state 5; via=<model1>; into=r } }
    3. graph = { f = fun r -> Sub { from=state 5; via=<model1>; into=Sub { from=state 6; via=<model2>; into=r } } }
    4. graph.f (<model2>) ==> Sub { from=state 5; via=<model1>; into=Sub { from=state 6; via=<model2>; into=<model2> } }

Instead of `state 5/6`, we'd actually have something more like
`Leaf0 { ... }`, but that's not really relevant. We also do some
optimizations if `sub`ing isn't actually necessary.

Handle is much simpler: we run our `Value.t` construction function (`f`)
on a fresh `graph`, which becomes constructed via calls to `perform`
presumably inside `f`. Then, we call `graph.f` on that result, so that
it is inside the nested `Sub`s, giving it access to those memoized
aliases. Finally, we wrap this in a `Computation.t`, which can be
`perform`ed into other computations.

```{=html}
<aside>
```
Instead of this continuation passing thing, why can't we just eagerly
construct the `Computation.t` graph as we go? That would require making
all the record fields of the `Computation.t` variants mutable, which
make some very, very scary bugs possible.
```{=html}
</aside>
```
A consequence of `perform`'s implementation is that the shape of
`Computation.t` under the Cont API is very linear: with the exception of
`Switch`/`Assoc` nodes, it is a chain of `Sub`s, each with more-or-less
a single node on the `From` side. You can think of this as a list of all
the things that have been instantiated and memoized, in the order of
instantiation.

### Why is `Bonsai.map` safe now?

But why can we now use `Bonsai.map` instead of `let%sub` and `let%arr`
combos? After all, we're not passing `graph` to `let%map`... How could
it construct `Sub` nodes and add them into the graph?

In short, We cheat and make `graph` global. We still require users to
pass it when possible, so that creating costly stateful nodes is an
explicit operation.
