# 03 - Incrementality

In the last 2 chapters, we learned how to build functional web UIs with
`virtual_dom`, and schedule side effects in response to user interaction
with `Effect.t`.

For applications with a large amount of frequently-changing input data,
it's important that we only re-compute the parts of the application that
actually depend on the new data. In this chapter, we'll:

-   Learn how to build and compose incremental computations via the
    `Bonsai.t` type and `let%arr` operator
-   Note that the Directed Acyclical Graph (DAG) of `Bonsai.t`s is
    actually static

## `Bonsai.t`

Bonsai is all about constructing graphs of incremental nodes. Some of
these are stateful, but most are derived as a function of the current
values of other nodes. A good analogy to help understand Bonsai is that
of the spreadsheet. From our blog post introducing the [Incremental
library](https://blog.janestreet.com/introducing-incremental/):

> In a spreadsheet, each cell contains either simple data, or an
> equation that describes how the value in this cell should be derived
> from values in other cells. Collectively, this amounts to a
> graph-structured computation, and one of the critical optimizations in
> Excel is that when some of the cells change, Excel only recomputes the
> parts of the graph that depend on those changed cells.

A `'a Bonsai.t` is a node in the incremental graph, kind of like a cell
in a spreadsheet.

`val Bonsai.return : 'a -> 'a t` wraps a plain OCaml value in a
`Bonsai.t`. This is like an Excel cell that contains some constant
value.

To create a new `Bonsai.t` as a function of other `Bonsai.t`s, we can
use the `let%arr` operator. It works just like [`ppx_let`'s
`let%map`](https://blog.janestreet.com/let-syntax-and-why-you-should-use-it/),
but with some extra performance optimizations for pattern matching on
incremental values. This is like a formula cell in Excel.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/incrementality_examples.ml,part=int_view -->
```
``` ocaml
let int_view (a : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr a = a in
  Vdom.Node.div [ Vdom.Node.text (Int.to_string a) ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#int_view">
```
```{=html}
</iframe>
```
`let%arr` is just pretty syntax for
`val Bonsai.map : 'a t -> f:('a -> 'b) -> 'b t`. It's ok to use
`Bonsai.map` directly, but `let%arr` is usually more ergonomic,
especially when mapping multiple `Bonsai.t`s together:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/incrementality_examples.ml,part=sum_and_display -->
```
``` ocaml
let sum_and_display (a : int Bonsai.t) (b : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr a = a
  and b = b in
  Vdom.Node.textf "%d + %d = %d" a b (a + b)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#sum_and_display">
```
```{=html}
</iframe>
```
For incrementality to be useful, inputs need to actually change. On to
[Chapter 4: state](./04-state.mdx)!

## The Underlying Machinery

`Bonsai.t` is actually a wrapper around [Incremental's
`Incr.t`](https://blog.janestreet.com/introducing-incremental/). The
biggest user-facing difference is that there is no `Bonsai.bind`, which
forces the computation graph to have a static shape. This enables some
[useful features and performance
optimizations](../advanced/why_no_bind.mdx). We'll learn how to write
control flow code without `bind` in a [later
chapter](./05-control_flow.mdx).
