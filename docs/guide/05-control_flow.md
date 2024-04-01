```{=html}
<!-- Here be dragons! Virtual_dom doesn't play well with MDX, so we make a fake library
to run the code examples below.

```ocaml
open! Core
module Bonsai = Bonsai.Cont
open! Bonsai.Let_syntax

module Vdom = struct
  module Node : sig
      type t

      val none : t

      val div : t list -> t
  end = struct
      type t = unit

      let none = ()
      let div _ = ()
  end
end

module State_examples = struct
  let counter_ui (local_ graph) =
    Bonsai.return Vdom.Node.none
end

```

-->
```
# 05 - Control Flow

In [chapter 3](./03-incrementality.mdx), we learned how to build and
compose a static graph of incremental `Bonsai.t`s using the `let%arr`
operator. But often, web UIs need to express some dynamic patterns, and
`let%arr` just isn't enough. In this chapter, we'll:

-   Use `match%sub` to conditionally evaluate `Bonsai.t`s
-   Evaluate a collection of `Bonsai.t`s separately for each of a
    dynamically-sized number of inputs
-   Learn what it means for a `Bonsai.t` to be "active" vs "inactive"
-   Remark on higher-order functions in Bonsai

## `match%sub`

Let's say we want to show the counter we built in [the state
chapter](./04-state.mdx) only when `show: bool Bonsai.t` is true. With
the tools we have today, we might write:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show_naive -->
```
``` ocaml
let maybe_show_naive show (local_ graph) =
  let counter = counter ~step:(return 1) graph in
  let%arr counter = counter
  and show = show in
  match show with
  | false -> Vdom.Node.none
  | true -> counter
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show_naive">
```
```{=html}
</iframe>
```
But because we are `let%arr`-ing on `counter`, the incremental runtime
will continuously recompute it, even when we aren't actually using it.

### Conditional Recomputation

We can avoid this and get a performance boost using Bonsai's
`match%sub`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show -->
```
``` ocaml
let maybe_show show (local_ graph) =
  let counter = counter ~step:(return 1) graph in
  match%sub show with
  | false -> Bonsai.return Vdom.Node.none
  | true -> counter
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show">
```
```{=html}
</iframe>
```
`match%sub` is like `match`, but for `Bonsai.t`s:

1.  The matched value should be a `'a Bonsai.t` or a literal tuple of
    `Bonsai.t`s.
2.  The values produced by each of the match-arms must be of type
    `'b Bonsai.t`.
3.  Any identifiers bound during matching are available as
    `'c Bonsai.t`s inside the arms. (You can access them as plain `'c`
    in guard clauses though.)
4.  The overall type of the `match%sub` expression has type
    `'b Bonsai.t`.

### Conditional Instantiation

`match%sub` has a superpower: you can use `graph` inside its arms. This
means we can instantiate some state that is local to one arm:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show_2 -->
```
``` ocaml
let maybe_show_2 show (local_ graph) =
  match%sub show with
  | `Count_by_1 -> counter ~step:(return 1) graph
  | `Count_by_2 -> counter ~step:(return 2) graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show_2">
```
```{=html}
</iframe>
```
Note that each branch has an independent counter with its own state.
You'll see this if you increment the first counter and then switch to
the second.

Interestingly, state does not go away when a branch ceases to be active:
as we noted [last chapter](./04-state.mdx), this is because Bonsai
maintains a central copy of the entire application state.

```{=html}
<aside>
```
Bonsai provides some [lifecycle functions](../how_to/lifecycles.mdx) to
schedule effects when a code block becomes active or inactive.
```{=html}
</aside>
```
### Conditional Data Dependencies

We can also use `match%sub` to pattern-match just like regular `match`,
allowing us to conditionally access data:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show_var -->
```
``` ocaml
let maybe_show_var show (local_ graph) =
  match%sub show with
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show_var">
```
```{=html}
</iframe>
```
Note that all cases of `Count_by`, share the same counter state. That's
because they all go to the same branch of the `match%sub`. If we wanted
to create separate versions of state for individual cases of `step`, we
could use guard clauses to create multiple branches that match the same
pattern, each with their own locally instantiated state:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show_var_guard -->
```
``` ocaml
let maybe_show_var_guard show (local_ graph) =
  match%sub show with
  | `Count_by step when Int.equal step 1 -> counter ~step graph
  | `Count_by step when Int.equal step 4 -> counter ~step graph
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show_var_guard">
```
```{=html}
</iframe>
```
This particular case is pretty silly: we're not going to write separate
`match%sub` branches for every potential value of `int`. Instead, we
could use [`scope_model`](../how_to/state_per_key.mdx), which maintains
separate copies of state for some value of a key:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=maybe_show_var_scope_model -->
```
``` ocaml
let maybe_show_var_scope_model show (local_ graph) =
  match%sub show with
  | `Count_by step ->
    Bonsai.scope_model
      (module Int)
      ~on:step
      ~for_:(fun (local_ graph) -> counter ~step graph)
      graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#maybe_show_var_scope_model">
```
```{=html}
</iframe>
```
## Creating a Dynamic Number of `Bonsai.t`s

In the [last chapter](./04-state.mdx), we created two separate counters
by calling `counter_ui graph` twice. But what if we want to create `n`
counters, where `n` is an `int Bonsai.t` that can change at runtime?

Let's try to build this with the tools we have:

``` ocaml
# let multiple_counters (n : int Bonsai.t) (local_ graph) =
  let%arr n = n in
  let (counters : Vdom.Node.t Bonsai.t list) =
    List.init n ~f:(fun _ -> State_examples.counter_ui graph)
  in
  let%arr counters = Bonsai.all counters in
  Vdom.Node.div counters
Line 4, characters 56-61:
Error: The value graph is local, so cannot be used inside a closure that might escape.
Hint: The closure might escape because it is an argument to a tail call
```

As you can see above, this won't even compile: the content of `let%arr`
blocks is runtime code, so the `local_` mode bans you from using `graph`
within them. Furthermore, if this code compiled, the output *would* have
type `Vdom.Node.t Bonsai.t Bonsai.t`, which is illegal: remember, the
Bonsai computation graph has to be static.

Instead, we can use Bonsai's `assoc` primitive:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=assoc -->
```
``` ocaml
val assoc
  :  ('k, 'cmp) Bonsai.comparator
  -> ('k, 'v, 'cmp) Map.t Bonsai.t
  -> f:('k Bonsai.t -> 'v Bonsai.t -> local_ Bonsai.graph -> 'result Bonsai.t)
  -> local_ Bonsai.graph
  -> ('k, 'result, 'cmp) Map.t Bonsai.t
```

Bonsai evaluates the body of `f` exactly once when your app starts to
produce a `'v Incr.t -> 'result Incr.t` function. This new function is
then applied to each key/value pair in the input map when your app runs,
and to any new keys added to the input map.

```{=html}
<aside>
```
`Bonsai.comparator` is a first class module with a `type t` and a
`sexp_of` function.
```{=html}
</aside>
```
Each key/value pair in the output map has its own independent state and
dependencies. This means that if the input map is 100,000 elements
large, but only one of the keys has data that is changing frequently,
only that key's instance will be re-run to recompute the overall output.

Here's an example, which will make multiple copies of the counter we
implemented [last chapter](./04-state.mdx):

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=multiple_counters -->
```
``` ocaml
let multiple_counters (input : unit Int.Map.t Bonsai.t) (local_ graph) =
  let counters =
    Bonsai.assoc
      (module Int)
      input
      ~f:(fun key (_ : unit Bonsai.t) (local_ graph) ->
        let%arr key = key
        and counter = State_examples.counter_ui graph in
        Vdom.Node.tr
          [ Vdom.Node.td [ Vdom.Node.textf "counter #%d:" key ]
          ; Vdom.Node.td [ counter ]
          ])
      graph
  in
  let%arr counters = counters in
  Vdom.Node.table (Map.data counters)
;;
```

Let's try it out!

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/control_flow_examples.ml,part=multiple_counters_dynamic -->
```
``` ocaml
let multiple_counters_dynamic graph =
  let counter_view, n = State_examples.counter ~step:(Bonsai.return 1) graph in
  let map_containing_n_entries =
    let%arr n = n in
    if n <= 0
    then Int.Map.empty
    else List.init n ~f:(fun i -> i, ()) |> Int.Map.of_alist_exn
  in
  let%arr counter_view = counter_view
  and table = multiple_counters map_containing_n_entries graph in
  Vdom.Node.div [ counter_view; table ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#multiple_counters_dynamic">
```
```{=html}
</iframe>
```
Note that if you add, remove, and re-add a counter, it will retain its
state.

```{=html}
<aside>
```
If your `Bonsai.assoc` produces `Vdom.Node.t`s, you might want to use
[`vdom_node_with_map_children`](./01-virtual_dom.mdx#diffing-lists) for
more efficient and stable diffing.
```{=html}
</aside>
```
## Further Reading

-   `match%sub` and `Bonsai.assoc` are [higher-order
    functions](../how_to/higher_order_functions.mdx)
-   The code inside `match%sub` branches or `assoc` can [become
    inactive](../how_to/lifecycles.mdx).
