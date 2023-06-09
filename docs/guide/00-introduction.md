# 00 - Introduction

This guide will teach you how to build user interfaces in the browser
using several libraries, primarily `Bonsai` and `Virtual_dom`. Together,
these libraries allow you to build applications in a functional style
instead of the imperative style encouraged by the browser's API.

In this guide we aim to explain how to use Bonsai, and, to a lesser
degree, how Bonsai works under the hood. We hope that the latter will
equip you with the knowledge necessary to tune the performance of your
applications.

# Web Apps at 10,000 Feet

The browser understands three languages: Javascript, HTML, CSS. Jane
Street programmers only understand one language: OCaml. Thus, we've made
it possible to write all three of the browser languages using OCaml.

-   `js_of_ocaml` is an OCaml-to-Javascript compiler.
-   `virtual_dom` is a library for building values that represent a
    chunk of HTML.
-   `css_gen` is a library for writing CSS styles in a type safe manner.

The CSS situation is a little more nuanced, since we actually recommend
writing CSS directly using `ppx_css`.

A user interface is a function from *data* to *view*. In types:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
(* Virtual_dom.Vdom.Node.t represents your application's view *)
open Virtual_dom

val ui : Your_input_type_here.t -> Vdom.Node.t
```

It's easy to write composable views with such functions, since all you
need to return is a plain old OCaml value. A small amount of boilerplate
can turn this function into a simple web app that continuously displays
the result of the function.

Of course, this is a huge simplification; in a real app, you usually
want:

-   *Interactivity*, so the user can click on, type into, and navigate
    through things.
-   *Incrementality*, so that large amounts of highly dynamically data
    can be displayed without the interface lagging.

Bonsai provides these features while still encouraging the composition
and abstraction properties of regular OCaml code. Bonsai wants you to
forget it is there. The signature of a Bonsai app looks a bit like this:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
open Bonsai_web

val ui : Your_input_type_here.t Value.t -> Vdom.Node.t Computation.t
```

It's just like before, except the input is wrapped with `Value.t` and
the output is wrapped with `Computation.t`. While there is slightly more
friction, writing re-usable UI components is just as easy. In addition,
we've expanded the kinds of components you can write, since
`Computation.t` encapsulates incremental state machines, which is how
interactivity is added to an interface.

Both these types are covered in detail in chapters [2](./02-dynamism.md)
and [3](./03-state.md).

# The Underlying Machinery

The incrementality in Bonsai comes from the `Incremental` library. When
a web page loads, Bonsai compiles the top-level
`Vdom.Node.t Computation.t` into something akin to `Vdom.Node.t Incr.t`.
Then the `Incr_dom` library handles running the main loop to keep the
incremental graph stabilized (i.e. up-to-date).

The `Vdom.Node.t` representing the current view gets put onto the screen
via a diff-and-patch process. The `virtual_dom` library always keeps
track of the previous `Vdom.Node.t` that it told the browser to display.
Whenever we request a *new* `Vdom.Node.t` to be displayed on the screen,
the library first compares it to the previous view to see what changed,
and then it applies *just those changes* to what the browser is
displaying.

Details regarding Incremental, and the virtual-dom diff-and-patch
strategy are abstracted away so you'll rarely need to think about them.
However, a good cost model will help you to avoid or debug performance
pitfalls. Throughout the rest of this guide, we will endeavor to provide
such a cost model.
