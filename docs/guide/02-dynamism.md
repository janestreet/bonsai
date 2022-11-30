# 02 - Dynamism

The previous chapter introduced an immutable view type, `Vdom.Node.t`
along with the idea that the UI is a function from data to view. For
large and dynamic input data, this function can be expensive and must run
quite often. To keep up with quickly changing inputs, we would like to
only re-compute the parts of the view that depend on newly changed data.

In this chapter, we'll:

- Explain the core Bonsai abstractions
- Learn how to use `let%sub` and `let%arr` operators to write Bonsai components
- Introduce a primitive for state
- See how Bonsai computations can use data from the outside world
- Discuss Bonsai more generally, beyond web UIs

# Values and computations

Bonsai is all about constructing incremental state machine graphs.
A good analogy to help understand Bonsai is that of the spreadsheet.
From our blog post introducing our [Incremental library](https://blog.janestreet.com/introducing-incremental/):

> In a spreadsheet, each cell contains either simple data, or an equation that
> describes how the value in this cell should be derived from values in other cells.
> Collectively, this amounts to a graph-structured computation, and one of the critical
> optimizations in Excel is that when some of the cells change, Excel only recomputes
> the parts of the graph that depend on those changed cells.

Bonsai is built around 2 fundamental types:

`'a Value.t` is like a spreadsheet cell. It can contain simple data or the output of some formula over other cells.
Critically for performance, if backed by a formula, it will only be recalculated if/when an input changes.
In Bonsai, any given `'a Value.t` is an instance of a component, such as a particular bit of vdom, piece of state,
or a combination of other `'a Value.t`s.

`'a Computation.t` is like the formula backing a spreadsheet cell. You can use the same formula in various cells,
and they will all function independently of each other. In Bonsai, it's the definition of a component, and how it's
composed of other components.

This can be difficult to grasp, so here's a few more analogies:

- An `'a Computation.t` is like the code for a function. A corresponding `'a Value.t` is like the memory cell containing the output of that function.
- An `'a Value.t` is an incremental computation that produces a value of type `'a`. An `'a Computation.t` is the structure/definition of that computation.

```{=html}
<aside>
```
If you're familiar with class-based React, this pattern isn't that different.
Classes define the structure and logic of components, and can use other classes in their implementation.
They are like `'a Computation.t`.

But at runtime, you're dealing with *instances* of components, which are created with JSX or `React.createElement()`.
These component instances have a lifecycle, and their view will be recomputed many times as state/input changes.
They are like `'a Value.t`.
```{=html}
</aside>
```

The motivation for having two types will be thoroughly explored at the end of this chapter.
For now, we'll learn how we can use these types to build Bonsai components.

# Writing Bonsai with `let%arr` and `let%sub`

Let us start with something basic: defining a computation that computes a
value that depends on two other values.


```{=html}
<aside>
```
If you're not familiar with the [`ppx_let`](https://github.com/janestreet/ppx_let) monadic let-binding style,
read [this article](https://blog.janestreet.com/let-syntax-and-why-you-should-use-it/) for an intro.
```{=html}
</aside>
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=juxtapose_part_1 -->
```
``` ocaml
let juxtapose_digits ~(delimiter : string) (a : int Value.t) (b : int Value.t)
  : string Computation.t
  =
  let%arr a = a
  and b = b in
  Int.to_string a ^ delimiter ^ Int.to_string b
;;
```

In short, `let%arr` creates a new `'b Computation.t` from:

- Some `'a Value.t`s, which will be incremental inputs to this computation, when instantiated
- An implementation function (`'a -> 'b`), which maps the "unwrapped" raw OCaml input values to the output of the computation

Here, `a = a and b = b` means that the `a` and `b` `'int Value.t`s will be unwrapped,
and available in the implementation function as `a` and `b` `int`s respectively.

The type of this entire `let%arr` expression, which includes the stuff on
both sides of `in`, is `string Computation.t` rather than
`string Value.t`. This means that the we've defined a blueprint for an incremental computation,
not an instance of the computation.

To instantiate our new component for use in another computation, we can use `let%sub`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=juxtapose_part_2 -->
```
``` ocaml
let _juxtapose_and_sum (a : int Value.t) (b : int Value.t) : string Computation.t =
  let%sub juxtaposed = juxtapose_digits ~delimiter:" + " a b in
  (* Within this scope, `val juxtaposed: string Value.t` *)
  let%sub sum =
    let%arr a = a
    and b = b in
    Int.to_string (a + b)
  in
  (* Computations don't NEED to be defined in separate declarations.
   * In the few lines above, we've defined and instantiated
   * an inline `string Computation.t` into a `string Value.t`. *)
  let%arr juxtaposed = juxtaposed
  and sum = sum in
  juxtaposed ^ " = " ^ sum
;;
```

`let%sub` takes an `'a Computation.t` and instantiates it into an `'a Value.t`,
which **must** then be used to construct a new `'b Computation.t`.

In the example above, we used `let%sub` to instantiate the `string Value.t`s
`sum` and `juxtaposed`, and then used `let%arr` to create a new computation with
those values as inputs. This is a common pattern you'll see in most Bonsai components.

A subtle, yet extremely important aspect of `let%sub` is that it makes a
copy of the input computation. This means that if you use `let%sub` twice on the
same `'a Computation.t`, you get the result nodes for two **independent** incremental computations.
All we've encountered so far are pure function computations constructed with
`let%arr`, so having multiple copies of a graph doesn't matter: all
the copies will always be producing identical results. Copying vs non-copying behavior is crucial
when computations contain internal state.


```{=html}
<aside>
```
Theoretically, `Value.t` is an Applicative, meaning that you can also use a `Value.map` operator
in addition to the `arr` and `sub` we've covered. In 99.9% of cases, we DO NOT recommend using it,
as it's easy to accidentially duplicate work. See [this blog post](../blogs/letsub.mdx) for more details.
```{=html}
</aside>
```
# State in Bonsai

The following example demonstrates how to use `Bonsai.state`, a
primitive computation for introducing internal state to a computation.
Notice that we get access to two result nodes: `count` is the state's
current value and `set_count` is a function for updating that value.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=counter_button -->
```
``` ocaml
let (counter_button : Vdom.Node.t Computation.t) =
  let%sub count, set_count = Bonsai.state (module Int) ~default_model:0 in
  let%arr count = count
  and set_count = set_count in
  (* view-construction logic *)
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> set_count (count + 1)))
        [ Vdom.Node.text "increment count" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counter_button">
```
```{=html}
</iframe>     
```

Note that `val counter_button: Vdom.Node.t Computation.t`, since the incrementally
computed "output" is the virtual-DOM for the counter.

Now we can see the significance of being able to instantiate a component
multiple times. The following code demonstrates that we can use `let%sub` on
`counter_button` to get three independent counters.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=three_counters -->
```
``` ocaml
let (three_counters : Vdom.Node.t Computation.t) =
  let%sub counter1 = counter_button in
  let%sub counter2 = counter_button in
  let%sub counter3 = counter_button in
  let%arr counter1 = counter1
  and counter2 = counter2
  and counter3 = counter3 in
  Vdom.Node.div [ counter1; counter2; counter3 ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#three_counters">
```
```{=html}
</iframe>
```

Every time we instantiate `counter_button` with `let%sub`, we get an
independent `Vdom.Node.t Value.t`. We use `Vdom.Node.div` to build
a user interface that contains all three buttons so the user can click
on them; however, first we need to use `let%arr` to get access to the
view inside each counter graph node.

The role of `let%sub` in Bonsai is similar to the `new` keyword in an
object-oriented programming language. Just like `new` makes a brand new
copy of the specified class with its own independent mutable fields, 
`let%sub` makes a brand new copy of the specified computation
with its own independent internal state. In addition, just like `new`
usually yields a reference/pointer (in languages like C# or Java)
instead of the data (type `'a`) itself,`let%sub` yields an `'a Value.t`.

We've introduced two basic kinds of computations - state, which may be
introduced by `Bonsai.state`, and work/logic, which may be introduced by
`let%arr`. While these are certainly the most important, Bonsai provides
primitive computations for a few other things, such as time-varying and
edge-triggering computations.

We've also introduced the primary means by which you construct larger
computations from smaller ones - `let%sub`. Part of the learning curve
of building Bonsai apps is getting comfortable composing together a
bunch of little computations.

<!-- With the changes I'm proposing above, the graph vs node analogy
no longer applies. Since this article is getting pretty long, I've instead
added an aside after `sub`, linking to the `letsub` blog article for an explanation of
why `sub` + `arr` is generally better than `map`.
 -->

# Inputs to the graph

Web apps are almost never self-contained systems: we need to interact
with external servers and data sources via RPC calls to get data from,
and to mutate, the outside world.

At runtime, a Bonsai app is a running incremental computation graph.
Just like data cells in a spreadsheet, our graph will have mutable
"source" nodes that are inputs to our incremental computation.

These nodes are of type `'a Var.t`: the third main
type in Bonsai. A var is similar to a `ref` or the analogous
`'a Incr.Var.t` from incremental.

```{=html}
<!-- $MDX file=../../src/var.mli,part=var_module_signature -->
```
``` ocaml
type 'a t

(** Creates a var with an initial value. *)
val create : 'a -> 'a t

(** Runs a function over the current value and updates it to the result. *)
val update : 'a t -> f:('a -> 'a) -> unit

(** Change the current value. *)
val set : 'a t -> 'a -> unit

(** Retrieve the current value. *)
val get : 'a t -> 'a

(** Get a value that tracks the current value, for use in a computation. *)
val value : 'a t -> 'a Value.t
```

The typical use-case for a var is that there is some source of
ever-changing data, such as a `Polling_state_rpc` from a server. The
Bonsai app will subscribe to these changes with a callback that updates
the var with the new data that it received. The main app computation
then receives the value-ified var after it has been passed through
`Var.value`. Here is a concrete example:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=counter_var -->
```
``` ocaml
let counter_every_second : int Value.t =
  let counter_var : int Bonsai.Var.t = Bonsai.Var.create (-1) in
  every (Time_ns.Span.of_sec 1.0) (fun () ->
    Bonsai.Var.update counter_var ~f:(fun i -> i + 1));
  Bonsai.Var.value counter_var
;;

let view_for_counter : Vdom.Node.t Computation.t =
  let%arr counter = counter_every_second in
  Vdom.Node.textf "counter: %d" counter
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counter">
```
```{=html}
</iframe>
```
# Bonsai is a compiler

It might surprise you that Bonsai itself does not implement incremental
computation OR a vdom runtime. That happens in the `Incremental` and
`Virtual_dom` libraries, respectively.

The `Bonsai` library provides syntax for describing the structure of an
incremental state machine. It then compiles the value and computation
"surface syntax" into the "assembly language" provided by the `Incremental` library.
Compilation happens once when the app starts up, and thereafter the main program only
interacts with the app in `Incr.t` form.

The Bonsai API is carefully designed to allow its compiler to statically
analyze the entire computation graph. This is why we [don't provide
a monadic bind](../blogs/why_no_bind.md), since the callback passed to `bind` is
an opaque function. There are few important consequences of the static
analyzability of Bonsai graphs:

-   Compilation to incremental nodes only needs to happen once, at
    startup.
-   We can run "whole-program analysis" on the graph to optimize and
    seriously condense the computation graph.
-   We have the ability to instrument each node in a computation with
    performance and debugging info. Eventually we plan to use this info
    to implement a debugger and profiler for Bonsai computations.

You can learn more about Bonsai as a general compiler for incremental state machines in [Bonsai Beyond the Web](11-beyond-web.md).
