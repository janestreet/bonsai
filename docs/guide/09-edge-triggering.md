# 09 - Edge Triggering

Bonsai encourages declarative UI construction. A computation is defined
as a list of dependencies and a function which reads the current value
of those dependencies, producing a new value. A computation defined in
this way doesn't care what the previous values of its dependencies were;
it always operates on their current value.

However, sometimes it can be helpful to witness a transition from one
value to another. In Bonsai, we have the
[`Bonsai.Edge`](https://ocaml.org/p/bonsai/v0.16.0/doc/Bonsai/Edge/index.html)
module, which has a collection of functions which can notice things like

1.  the passage of time
2.  the activation and deactivation of components
3.  changing of the contents of a Value.t

and schedule Effects when they occur.

## `after_display`

The main `Edge` function we'll take a look at is
`Bonsai.Edge.lifecycle`, which takes a number of optional parameters of
type `unit Effect.t Value.t`. The first of these is `after_display`.
`Edge.lifecycle` schedules the effect passed in via `after_display` as
the last operation in the Bonsai render-loop, right after the DOM has
been updated.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/edge_examples.ml,part=after_display -->
```
``` ocaml
let frame_counter =
  let%sub frames, set_frames = Bonsai.state 0 in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~after_display:
        (let%map frames = frames
         and set_frames = set_frames in
         set_frames (frames + 1))
      ()
  in
  let%arr frames = frames in
  Vdom.Node.textf "this component has been alive for %d frames" frames
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#after-display">
```
```{=html}
</iframe>
```
The text I chose for that component was very intentional. I wrote "this
component has been alive for {n} frames" instead of "the application has
been running for {n} frames". This is because Edge functions only run if
their computation is active. Let's start with a demo, and then discuss
what "active" means.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/edge_examples.ml,part=only_when_active -->
```
``` ocaml
let frame_toggler =
  let%sub showing, set_showing = Bonsai.state false in
  let%sub output =
    match%sub showing with
    | true -> frame_counter
    | false -> Bonsai.const Vdom.Node.none
  in
  let%arr showing = showing
  and set_showing = set_showing
  and output = output in
  let toggle_showing = set_showing (not showing) in
  let button_text = if showing then "disable counter" else "enable counter" in
  let toggle_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> toggle_showing) ]
      [ Vdom.Node.text button_text ]
  in
  Vdom.Node.div [ toggle_button; output ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#only-when-active">
```
```{=html}
</iframe>
```
If you disable the component (wait a few seconds), you'll notice that
the counter picks up where it left off rather than continuing in the
background.

As mentioned earlier, `after_display` only runs when the computation is
"active", and as this example demonstrates, being inside of a
`match%sub` is one way to change the activity status of a computation.

In fact, aside from `match%sub`, there's only one other combinator that
influences the active status: `Bonsai.assoc`.

```{=html}
<aside>
```
Technically, `if%sub` and `Bonsai.enum_` also have this property, but in
reality, `if%sub` and `match%sub` are implemented in terms of `enum_`,
so they're all counted together.
```{=html}
</aside>
```
`Bonsai.assoc` is used to build a dynamic number of instances of a
computation.

Just like how

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let%sub a = my_component in
let%sub b = my_component in
```

will create two *distinct* instances of `my_component`, each with their
own state, `Bonsai.assoc` can instantiate a dynamic number of
computations, one instantiation per key-value pair from an incoming
`_ Map.t Value.t`.

I have a small library,
[Bonsai_web_ui_extendy](https://ocaml.org/p/bonsai/v0.16.0/doc/Bonsai_web_ui_extendy/index.html),
which uses `assoc` to implement a component for easily creating and
deleting instances of another component.

We'll reuse the `frame_counter` component built in the first example,
and combine it with `extendy` to get multiple `frame_counter`s.

Let's see it in use:
`<!-- $MDX file=../../examples/bonsai_guide_code/edge_examples.ml,part=extendy-use -->`{=html}

``` ocaml
let wrap_remove frame_counter remove =
  let x_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> remove) ]
      [ Vdom.Node.text "x" ]
  in
  Vdom.Node.div [ x_button; frame_counter ]
;;

let many_frame_watches =
  let%sub { contents; append; _ } = extendy frame_counter ~wrap_remove in
  let%arr contents = contents
  and append = append in
  let append_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> append) ]
      [ Vdom.Node.text "add" ]
  in
  Vdom.Node.div (append_button :: Map.data contents)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#extendy-use">
```
```{=html}
</iframe>
```
By clicking on the "add" button, we create multiple frame-counters, each
with their own state, each which began counting at the moment of their
creation. It might not be obvious, but clicking on the `x` button not
only removes the component from the view, but from the entire Bonsai
computation graph, so the `on_display` effect is also stopped entirely.

## `on_activate` / `on_deactivate`

The other two optional parameters to `Bonsai.Edge.lifecycle` are
`on_activate` and `on_deactivate`, both of which share the same type as
`after_display`: `unit Effect.t Value.t`. These effects are run whenever
the lifecycle computation becomes active or inactive.

```{=html}
<aside>
```
By incorporating a `lifecycle` computation into a component of yours,
the `on_activate` / `on_deactivate` callbacks are effectively measuring
the activation/deactivation of the containing component.
```{=html}
</aside>
```
Let's modify the lifecycle component to use these new functions. First,
though, we'll want to do something when the activation/deactivation
occurs. For that, I built a tiny logging component which will let me
append a list of strings sent by the `frame_counter` component.

Ok, on to the extension of `frame_counter`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/edge_examples.ml,part=activations -->
```
``` ocaml
let frame_counter (log : (string -> unit Ui_effect.t) Value.t) =
  let%sub frames, set_frames = Bonsai.state 0 in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map log = log in
         log "🚀")
      ~on_deactivate:
        (let%map log = log in
         log "🔥")
      ~after_display:
        (let%map frames = frames
         and set_frames = set_frames in
         set_frames (frames + 1))
      ()
  in
  let%arr frames = frames in
  Vdom.Node.textf "this component has been alive for %d frames" frames
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#extendy-use-2">
```
```{=html}
</iframe>
```
```{=html}
<aside>
```
If you don't like the look of

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let%map log = log in
log "🔥"
```

you could also write it as

```{=html}
<!-- $MDX skip -->
```
``` ocaml
log >>| Fn.( |> ) "🔥"
```

`<br/>`{=html}

```{=html}
<aside>
```
Please don't though
```{=html}
</aside>
```
```{=html}
</aside>
```
## `on_change`

With the `lifecycle` function as a primitive, we can implement other
useful edge-triggering functions. One of these is also included in the
`Bonsai.Edge` module: `on_change'`.

`on_change'` monitors a `'a Value.t`, and when that value changes, it
calls a user-provided function, giving that function both the previous
and current value. This user-provided function returns an `Effect.t`,
which will be scheduled whenever the value changes.

```{=html}
<aside>
```
You currently have all the tools to implement `on_change'` yourself, and
you can find the implementation
[here](https://github.com/janestreet/bonsai/blob/master/src/proc.ml#L832).
```{=html}
</aside>
```
Combining the counter-component from [Chapter 3](./03-state.mdx) and the
logging component that I used above, we can write a component which
contains both a counter and a log, where the log is updated when the
value changes.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/edge_examples.ml,part=logging_counter -->
```
``` ocaml
let logging_counter =
  let%sub log_view, log = logger in
  let%sub counter_view, counter = counter in
  let%sub () =
    let callback =
      let%map log = log in
      fun prev cur ->
        match prev with
        | None -> Ui_effect.Ignore
        | Some prev -> log (if prev < cur then "🚀" else "🔥")
    in
    Bonsai.Edge.on_change'
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      counter
      ~callback
  in
  let%arr log_view = log_view
  and counter_view = counter_view in
  Vdom.Node.div [ counter_view; log_view ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#logging-counter">
```
```{=html}
</iframe>
```
## Implications for intelligibility and testing

Declarative programs are easy to reason about and test. Extensive use of
the `Edge` module will make your program less and less declarative.

Every time that you have the opportunity, you should opt for using
anything other than an `Edge.*` function.

However, sometimes it's necessary, and we have testing helpers to make
your life a bit easier when you do use edge triggering. Because
`after_display` runs its effect, well, after the display has occurred,
how would this interact with Bonsai testing functions, like
`Handle.show`?

To demonstrate, we'll build an *awful* Bonsai component: a linear chain
of `on_changes`:

```{=html}
<!-- $MDX file=../../test/of_bonsai_itself/test_proc_bonsai.ml,part=chain-computation -->
```
``` ocaml
let chain_computation =
  let%sub a = Bonsai.const "x" in
  let%sub b, set_b =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub c, set_c =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub d, set_d =
    Bonsai.state " " ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      a
      ~callback:set_b
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      b
      ~callback:set_c
  in
  let%sub () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      c
      ~callback:set_d
  in
  return (Value.map4 a b c d ~f:(sprintf "a:%s b:%s c:%s d:%s"))
;;
```

Because `on_change` triggers at the end of each frame, it should take 4
frames to settle. And indeed, in a unit test, that's exactly what we'll
see:

```{=html}
<!-- $MDX file=../../test/of_bonsai_itself/test_proc_bonsai.ml,part=chained-on-change -->
```
``` ocaml
let%expect_test "chained on_change" =
  let handle = Handle.create (Result_spec.string (module String)) chain_computation in
  Handle.show handle;
  [%expect {| a:x b:  c:  d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:  d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d: |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}];
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}]
;;
```

But `Bonsai_web_test.Handle` has a function that makes this a bit nicer:
`recompute_view_until_stable`, so we can rewrite the test in a way that
skips all the intermediate frames:

```{=html}
<!-- $MDX file=../../test/of_bonsai_itself/test_proc_bonsai.ml,part=chained-on-change-recompute -->
```
``` ocaml
let%expect_test "chained on_change with recompute_view_until_stable" =
  let handle = Handle.create (Result_spec.string (module String)) chain_computation in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {| a:x b:x c:x d:x |}]
;;
```

`recompute_view_until_stable` is handy, but it's hiding intermediate
states. If those intermediate states allow for logical bugs in your
application, then you might miss them. As mentioned above: avoid `Edge`
if you can; it's a *sharp* tool.
