# 02 - Dynamism

Dynamism is central to engaging applications: as the state of the world
changes, so should the UI.

The previous chapter introduced an immutable view type, `Vdom.Node.t`
along with the idea that the UI is a function from data to view. For
large and dynamic input data, this function is expensive and must run
quite often. To keep up with quickly changing data, we would like to
only re-compute the parts of the view that depend on newly changed data.

This chapter takes a detour from the theme of computing web UIs to
investigate the core Bonsai abstractions. It may be surprising to know
that Bonsai isn't specialized for user interfaces; rather, it answers
the very generic question of how to build composable incremental
state-machines. As it turns out, incremental state-machines are a great
abstraction for building UI!

```{=html}
<aside>
```
This chapter is more complicated than we might like due to a
longstanding quirk of Bonsai's architecture. We'll begin by describing
the more ideal way to think about Bonsai, and then we'll explain why it
isn't quite accurate.
```{=html}
</aside>
```
# Values and computations

Bonsai is all about constructing incremental state machine graphs. A
`'a Value.t` is a node in a graph that represents a `'a` that changes
over time. A `'a Computation.t` is an entire graph that might contain
many `Value.t` of different types, but culminates in a `'a Value.t`. The
motivation for having two types will be thoroughly explored later, but
let us start with something basic: building a graph that computes a
value that depends on two other values.

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

The two phrases `a = a` and `b = b` may look a little silly, but they
are necessary. The expression on the right-hand side of both bindings in
the `let%arr` has type `int Value.t`, but the pattern on the left hand
side is a plain old `int` that we can freely pass to `Int.to_string`. So
`let%arr` is useful for "unwrapping" the data inside a `Value.t` so that
we can access it for a limited scope.

The type of the entire `let%arr` expression, which includes the stuff on
both sides of `in`, is `string Computation.t` rather than
`string Value.t`. This means that the result is a graph and not a node
in a graph. To obtain the final node of a `Computation.t` graph, we can
use a `let%sub` expression.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=juxtapose_part_2 -->
```
``` ocaml
let _juxtapose_and_sum (a : int Value.t) (b : int Value.t) : string Computation.t =
  let%sub juxtaposed = juxtapose_digits ~delimiter:" + " a b in
  let%sub sum =
    let%arr a = a
    and b = b in
    Int.to_string (a + b)
  in
  let%arr juxtaposed = juxtaposed
  and sum = sum in
  juxtaposed ^ " = " ^ sum
;;
```

We provide a computation and `let%sub` provides a name we can use to
refer to the result node of that computation. In the first `let%sub`
above, the computation is `juxtapose_digits a b` and the name is
`juxtaposed`. The important thing about using `let%sub` is that
`juxtaposed` has type `string Value.t`, so we can freely use it in
`let%arr` expressions.

A subtle, yet extremely important aspect of `let%sub` is that it makes a
copy of the input computation, and the node that the name refers to is
the result node of that copy, rather than of the original. This means
that if you use `let%sub` twice on the same computation, you get access
to the result nodes for two independent copies of the same graph. All
we've encountered so far are pure function computations constructed with
`let%arr`, so having multiple copies of a graph is useless, since all
the copies will always be producing identical results. The ability to
copy is useful when computations contain internal state.

The following example demonstrates how to use `Bonsai.state`, a
primitive computation for introducing internal state to a computation.
Notice that we get access to two result nodes: `count` is the state's
current value and `set_count` is a function for updating that value.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=counter_button -->
```
``` ocaml
let (counter_button : Vdom.Node.t Computation.t) =
  let%sub count, set_count = Bonsai.state 0 in
  let%arr count = count
  and set_count = set_count in
  (* view-construction logic *)
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count + 1)) ]
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
Now we can illustrate the power of being able to instantiate a component
twice. The following code demonstrates that we can use `let%sub` on
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
Every time we instantiate `counter_button` with `let%sub`, we get a
`Vdom.Node.t Value.t` that represents the final result node of a copy of
the `counter_button` computation graph. We use `Vdom.Node.div` to build
a user interface that contains all three buttons so the user can click
on them; however, first we need to use `let%arr` to get access to the
view inside each counter graph node.

The role of `let%sub` in Bonsai is similar to the `new` keyword in an
object-oriented programming language. Just like `new` makes a brand new
copy of the specified class with its own independent mutable fields, so
also does `let%sub` make a brand new copy of the specified computation
with its own independent internal state. In addition, just like `new`
usually yields a reference/pointer (in languages like C# or Java)
instead of the data itself, so also does `let%sub` yield merely the
result node of the newly copied graph instead of the graph itself.

We've introduced two basic kinds of computations - state, which may be
introduced by `Bonsai.state`, and work, which may be introduced by
`let%arr`. While these are certainly the most important, Bonsai provides
primitive computations for a few other things, such as time-varying and
edge-triggering computations.

We've also introduced the primary means by which you construct larger
computations from smaller ones - `let%sub`. Part of the learning curve
of building Bonsai apps is getting comfortable composing together a
bunch of little computations.

# The scary side of values

The previous section intentionally did not explain that `Value.t` is an
applicative, which means that it works with the `let%map` syntax, in
addition to the `let%arr` syntax we've already introduced. The
difference between the two is very small: `let%arr` expands to the
expansion of `let%map`, except it wraps the entire thing in a call to
`return`. The following

`ocaml skip let f (x : int Value.t) : int Computation.t =   let%arr x = x in   x + 1`

expands to

`ocaml skip let f (x : int Value.t) : int Computation.t =   (let%arr x = x in      x + 1)`

which further expands to

`ocaml skip let f (x : int Value.t) : int Computation.t =   return (Value.map x ~f:(fun x -> x + 1))`

The `Value.t` applicative interface is scary because re-using the result
of a `let%map` expression causes the work that it represents to be
duplicated. Consider the following computation.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=problem_with_map_part_1 -->
```
``` ocaml
let component (xs : int list Value.t) : string Computation.t =
  let sum =
    let%map xs = xs in
    List.fold xs ~init:0 ~f:( + )
  in
  let average =
    let%map sum = sum
    and xs = xs in
    let length = List.length xs in
    if length = 0 then 0 else sum / length
  in
  let%arr sum = sum
  and average = average in
  [%string "sum = %{sum#Int}, average = %{average#Int}"]
;;
```

We would like this computation to only do the work of computing `sum`
once; however, every usage of `sum` entails an iteration through the
list. Note that the final result depends on `sum` directly, but also
indirectly through `average`; this means that `sum` is computed twice in
order to produce the formatted string.

This explanation seems to contradict the explanation in the beginning of
this chapter that computations are graphs and values are nodes in the
graph. The truth is that values are also graphs, and re-using a value
entails using another copy of that value's graph, thus duplicating any
work contained in the graph. To avoid this work duplication, we can
instantiate the value with `let%sub`, but since `let%sub` only
instantiates computations, we must wrap the `let%map` inside a call to
`return`. For consistency and robustness, we'll apply this
transformation to `average` as well, even though it is only used once.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=problem_with_map_part_2 -->
```
``` ocaml
let component (xs : int list Value.t) : string Computation.t =
  let%sub sum =
    return
      (let%map xs = xs in
       List.fold xs ~init:0 ~f:( + ))
  in
  let%sub average =
    return
      (let%map sum = sum
       and xs = xs in
       let length = List.length xs in
       if length = 0 then 0 else sum / length)
  in
  return
    (let%map sum = sum
     and average = average in
     [%string "sum = %{sum#Int}, average = %{average#Int}"])
;;
```

Before the introduction of `let%arr`, this was the idiomatic way of
using Bonsai. However, now that `let%arr` exists, we can transform the
above code into the following, exactly equivalent, computation:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamism_examples.ml,part=problem_with_map_part_3 -->
```
``` ocaml
let component (xs : int list Value.t) : string Computation.t =
  let%sub sum =
    let%arr xs = xs in
    List.fold xs ~init:0 ~f:( + )
  in
  let%sub average =
    let%arr sum = sum
    and xs = xs in
    let length = List.length xs in
    if length = 0 then 0 else sum / length
  in
  let%arr sum = sum
  and average = average in
  [%string "sum = %{sum#Int}, average = %{average#Int}"]
;;
```

While the `Value.t` applicative can have surprising behavior, if you
restrict yourself to only use `let%sub` and `let%arr`, then you won't
ever accidentally duplicate work.

# Inputs to the graph

Dynamic data flows into the graph through `'a Var.t`, the third main
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

The `Bonsai` library does not provide the logic for stabilizing an
incremental function and extracting the output value. Instead, it
compiles the value and computation "surface syntax" into the "assembly
language" provided by the `Incremental` library. Compilation happens
once when the app starts up, and thereafter the main program only
interacts with the app in `Incr.t` form.

The Bonsai API is carefully designed to allow its compiler to statically
analyze the entire graph. This is why we [don't provide
bind](../blogs/why_no_bind.md), since the callback passed to `bind` is
an opaque function. There are few important consequences of the static
analyzability of Bonsai graphs:

-   Compilation to incremental nodes only needs to happen once, at
    startup.
-   We can run "whole-program analysis" on the graph to optimize and
    seriously condense the computation graph.
-   We have the ability to instrument each node in a computation with
    performance and debugging info. Eventually we plan to use this info
    to implement a debugger and profiler for Bonsai computations.
