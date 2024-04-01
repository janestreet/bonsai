# Bonsai Docs: Guide (Introduction)

This guide will teach you how to build web UIs in OCaml. We'll learn how
to:

-   Write [HTML with `virtual_dom`](./01-virtual_dom.mdx), with
    interactivity powered by side-effects [encapsulated as
    `Effect.t`s](./02-effects.mdx)
-   Structure our web UI as a graph of composable, [incremental
    computations with `Bonsai.t`](./03-incrementality.mdx)
-   Instantiate and use [state](./04-state.mdx)
-   Conditionally evaluate Bonsai code, or create a dynamic number of
    `Bonsai.t`s, with [`match%sub` and `assoc`](./05-control_flow.mdx)

These are the basic tools of writing OCaml web UIs. To learn how to
[style with ppx_css](../how_to/css.mdx), [send RPCs to a
server](../how_to/rpcs.mdx), [test your Bonsai
code](../how_to/testing.mdx), and more, see the [Bonsai
how-tos](../how_to/readme.md).

```{=html}
<aside>
```
These docs do not attempt to teach
[HTML](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML)
or [CSS](https://developer.mozilla.org/en-US/docs/Learn/CSS/First_steps)
proficiency; there are plenty of resources on the internet that do it
better than we could.
```{=html}
</aside>
```
This guide is not intended to replace
[bonsai.mli](https://github.com/janestreet/bonsai/blob/master/src/bonsai.mli),
which lists and documents everything that Bonsai provides.

The rest of this intro previews building a simple web app in OCaml.
We'll discuss each step in depth in the guide chapters.

## OCaml Web Apps at 10,000 Feet

Functional web UIs are functions from *data* to a *view*.

The *data* can be client-side state, data embedded in the URL, data from
the server, etc.

The *view* is the part users see. In web UIs, the view is HTML (provided
by `virtual_dom`), styled by CSS.

For example, a web UI that tells a user how many unread emails they have
might look like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=message_vdom -->
```
``` ocaml
val message_vdom : name:string -> new_emails:int -> Vdom.Node.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=message_vdom -->
```
``` ocaml
open! Core
open Virtual_dom

let message_vdom ~name ~new_emails =
  Vdom.Node.div
    ~attrs:[ [%css {|font-size: 16px;|}] ]
    [ Vdom.Node.textf "hello %s! you have %d new emails" name new_emails ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#message_vdom">
```
```{=html}
</iframe>
```
User interactions, state updates, and RPC server calls are just *side
effects* of an otherwise pure function. We wrap these side effects in an
`Effect.t` type.

For example, we could add a button that "reads" an email to our UI:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=read_email_button -->
```
``` ocaml
val read_email_button : on_click:unit Effect.t -> Vdom.Node.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=read_email_button -->
```
``` ocaml
let read_email_button ~on_click =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text "Read an email!" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#read_email_button">
```
```{=html}
</iframe>
```
A desirable property is incrementality: when something changes, we only
recompute stuff that depends on it. We can do so by wrapping our inputs
and outputs in the incremental `Bonsai.t` type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=emails_bonsai -->
```
``` ocaml
val emails_bonsai
  :  name:string Bonsai.t
  -> new_emails:int Bonsai.t
  -> read_email_effect:unit Effect.t Bonsai.t
  -> Vdom.Node.t Bonsai.t
```

We can compose `Bonsai.t`s with the `let%arr` operator:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=emails_bonsai -->
```
``` ocaml
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let emails_bonsai ~name ~new_emails ~read_email_effect =
  let message =
    let%arr name = name
    and new_emails = new_emails in
    message_vdom ~name ~new_emails
  in
  let%arr message = message
  and read_email_effect = read_email_effect in
  Vdom.Node.div [ message; read_email_button ~on_click:read_email_effect ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#emails_bonsai">
```
```{=html}
</iframe>
```
In the code above, `message` will not be recomputed if only
`read_email_effect` changes.

But incrementality doesn't matter if we only have constants. Interesting
apps are stateful. We can use `Bonsai.state` to create a simple
getter/setter state. To use `Bonsai.state` and other `Bonsai.*`
primitives, we need a `local_ Bonsai.graph` "graph-builder", which
Bonsai will pass into your top-level `app` function.

In our email example, we can use `Bonsai.state` to keep track of how
many unread emails the user has and modify that count whenever they
"read" one:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=emails_stateful -->
```
``` ocaml
val emails_stateful : name:string Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=emails_stateful -->
```
``` ocaml
let emails_stateful ~name (local_ graph) =
  let default_count = 999 in
  let (count : int Bonsai.t), (set_count : (int -> unit Effect.t) Bonsai.t) =
    Bonsai.state default_count graph
  in
  let read_email_effect =
    let%arr count = count
    and set_count = set_count in
    set_count (count - 1)
  in
  emails_bonsai ~name ~new_emails:count ~read_email_effect
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#emails_stateful">
```
```{=html}
</iframe>
```
Note that the state "setter" is an incrementally computed function that
produces a `unit Effect.t`. When this effect is scheduled via an event
handler, the state will update.

And since our ultimate goal is to produce a single
incrementally-computed `Vdom.Node.t`, with state managed by Bonsai, a
complete app looks like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=app -->
```
``` ocaml
val app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=app -->
```
``` ocaml
let app (local_ graph) = emails_stateful ~name:(Bonsai.return "User") graph
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#app">
```
```{=html}
</iframe>
```
We can run it with:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let () = Bonsai_web.Start.start app
```

## Bonsai is Generic

Bonsai isn't actually web-specific: it's a library for building,
composing, and running pure, incremental, state-machines. It works
particularly well for web UIs, but it could also power other UI
backends, or even stateful, incremental computation on servers.

That's why instead of `open! Bonsai`, you'll `open! Bonsai_web`:
`Bonsai_web` contains a bunch of web-specific utils and helpers, in
addition to the core functionality in `Bonsai`.

## The Underlying Machinery

Browsers can only really run
[JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
and [WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly).
That's why we need
[js_of_ocaml](https://ocsigen.org/js_of_ocaml/latest/manual/overview),
which compiles OCaml bytecode to JavaScript, and provides bindings for
browser APIs.
