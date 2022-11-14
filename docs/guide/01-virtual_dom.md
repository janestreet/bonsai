# 01 - Virtual-dom

The *view* (stuff you can see) of a browser interface is
described by a tree of HTML *elements*, each of
which can have some *attributes* attached. The `virtual_dom` library
provides an OCaml interface for constructing these trees.

You can think of this as an alternate syntax for HTML: As a general rule, instead
of `<tag attr="value">children</tag>` we'll use `tag_func ~attr:(attr_func attr_val) children`.

In this chapter, we'll:

- Learn how to use the `virtual_dom` library to represent DOM structure
- Introduce interactivity via event handlers and the `Effect` type, and
- Discuss how this "virtual DOM" is actually rendered by the browser

# Vdom.Node.t

This wouldn't be a programming tutorial without a hello world example,
which introduces the `Vdom.Node.text` node constructor.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=hello_world -->
```
``` ocaml
let hello_world : Vdom.Node.t = Vdom.Node.text "hello world!"
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#hello_world">
```
```{=html}
</iframe>
```
These text nodes will frequently be the "leaf" of a view (there are no
"children" of a text node). For example, let's put some text inside a bulleted list
by using some more node constructors:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=bulleted_list -->
```
``` ocaml
let bulleted_list : Vdom.Node.t =
  let open Vdom.Node in
  div
    [ h3 [ text "Norwegian Pancakes" ]
    ; ul
        [ li [ text "3 eggs" ]
        ; li [ text "2 cups of milk" ]
        ; li [ text "1 cup of flour" ]
        ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#bulleted_list">
```
```{=html}
</iframe>
```
For the bulleted list, we use the `ul` and `li` functions. These
correspond to the [ul
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul)
and the [li
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li),
which MDN helpfully tells us stands for
`<u>`{=html}U`</u>`{=html}nordered `<u>`{=html}L`</u>`{=html}ist and
`<u>`{=html}L`</u>`{=html}ist `<u>`{=html}I`</u>`{=html}tem.
<!-- These inline HTML tags aren't showing up, just an fyi -->

`h3` is short for "header level 3", and is responsible for the larger
font in the title text, and `div` is a ["content
division"](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div)
and serves as a useful wrapper for the rest of the content.

```{=html}
<aside>
```
There is a `Vdom.Node.*` node constructor function for *almost* every
[HTML5 dom node](https://developer.mozilla.org/en-US/docs/Web/HTML/Element).
If a constructor is missing, `Vdom.Node.create` is available to manually
specify the tag, but the `Virtual_dom` maintainers gladly accept
contributions back to the main library!
```{=html}
</aside>
```
# Vdom.Attr.t

An optional argument to the `Vdom.Node.*` constructor functions is a
`Vdom.Attr.t list`. These `Attr.t` correspond to [DOM
attributes](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes),
[DOM
properties](https://developer.mozilla.org/en-US/docs/Web/API/Element#properties),
and [DOM
event_handlers](https://developer.mozilla.org/en-US/docs/Web/Guide/Events/Event_handlers).

Attributes can be used to tweak the appearance and behavior of the nodes
that they are attached to. For example, you can add placeholder text to a
textbox:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=input_placeholder -->
```
``` ocaml
let input_placeholder : Vdom.Node.t =
  Vdom.Node.input ~attr:(Vdom.Attr.placeholder "placeholder text here") ()
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#input_placeholder">
```
```{=html}
</iframe>
```
Or color text with inline css:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=css_gen -->
```
``` ocaml
let css_gen : Vdom.Node.t =
  Vdom.Node.span
    ~attr:(Vdom.Attr.style (Css_gen.color (`Name "red")))
    [ Vdom.Node.text "this text is red" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#css_gen">
```
```{=html}
</iframe>
```
[The 8th chapter "css"](./08-css.md) goes into much more depth on the
styling attributes. Otherwise, as with dom nodes, you can find a corresponding
`Vdom.Attr.*` for any HTML5 attribute or property.

Probably the most important attributes are "event handlers", which register functions
that are called when a user interacts with the element (like clicking on
buttons or typing into a text box). In other words, handler attributes are our primitive
for building interactive UIs.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=clicky_button -->
```
``` ocaml
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attr:
      (Vdom.Attr.on_click (fun (_evt : mouse_event) ->
         alert "hello there!";
         Ui_effect.Ignore))
    [ Vdom.Node.text "click me!" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clicky_button">
```
```{=html}
</iframe>
```
These functions usually receive a browser-level event value (ignored in
the above example as `_evt`) alongside any useful data extracted from
that event. For example, see the following event-handler attributes for
mouse-clicks and typing into a textbox:

`ocaml skip val Vdom.Attr.on_click : (mouse_event -> unit Vdom.Effect.t) -> Vdom.Attr.t`
`ocaml skip val Vdom.Attr.on_input : (input_event -> string -> unit Vdom.Effect.t) -> Vdom.Attr.t`

The return type for these event handler functions is
`unit Vdom.Effect.t`, which is the final type that we care about in the
`Virtual_dom` library.


```{=html}
<aside>
```
Note that in the signatures above, the `on_input` handler function is also
given a string that holds the current contents of that textbox.

This kind of design applies across most event handler `Vdom.Attr.*` signatures,
and allows us to write simpler handler functions that don't need to worry about extracting
data they need from the browser's low-level API.

As a result, the browser-level event-values like `mouse_event` and `input_event` are
almost always ignored in Bonsai apps.
```{=html}
</aside>
```

# unit Vdom.Effect.t

In the example above, the `on_click` handler function:

1. Called the `alert` browser API as a side effect
2. Returned `Vdom.Effect.Ignore`

What is this `Vdom.Effect.Ignore` value doing, and why must these event-handlers
return values of type `unit Vdom.Effect.t` in the first place?

In reality, values of type `unit Vdom.Effect.t` are used to schedule
work on Bonsai's event-queue. `Vdom.Effect.Ignore` is the no-op event,
and it schedules no work on the event-queue.
`Vdom.Effect.Many [a; b; c]` wraps up multiple events, scheduling them
all in order.

That leaves us with two more question:

1.  How do I get values of type `unit Vdom.Effect.t` that aren't just
    `Ignore` and `Many`
2.  Why would I want to use the Bonsai event queue anyway?

Both of which will be answered in [Bonsai Guide Part 3:
State](./03-state.md).

```{=html}
<aside>
```
`unit Vdom.Effect.t` is actually an alias for `unit Ui_effect.t`. They
are the same type, and it's merely re-exported from the `Vdom` library
as a convenience. If you see a `unit Ui_effect.t` (either in another
library, or from merlin), know that it is the same thing as
`unit Vdom.Effect.t`.
```{=html}
</aside>
```

# The Underlying Machinery

Historically, frontend logic worked by listening to browser-level events, and
manually modifying the DOM in response. This didn't scale well to complex behavior,
because state and logic were diffused through the entire application.

In contrast, modeling our UI as a function that returns a virtual-DOM view allows us
to clearly and exhaustively define possible states, how they should be handled, and
what the view should be in a declarative fashion. As we'll see, it also allows us to
break our UI down into reusable, composable components.

A virtual-DOM is an immutable tree of immutable UI elements that
represents the view of the application at a point in time. This is in
contrast to [the DOM (Document Object
Model)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model),
which is a mutable tree of mutable UI elements.

```{=html}
<aside>
```
The term "virtual-DOM" is not unique to Bonsai. Many other UI libraries
like [React](https://reactjs.org/docs/faq-internals.html),
[Elm](https://github.com/elm/virtual-DOM), and
[Vue](https://vuejs.org/v2/guide/render-function.html#The-Virtual-DOM)
have their own "virtual-DOM" libraries that all share similar goals.
```{=html}
</aside>
```

Essentially, the `Virtual_dom` library works by diff-ing the current and next
virtual-DOM trees, then applying those changes to the "real" DOM using the mutative
browser API. Because all this happens under the surface, Bonsai application developers
are solely concerned with defining the logic for how vdom views are produced.

Note that a new tree is generated every time the view changes.
While this may appear to be a performance nightmare, Bonsai uses incremental computation
to avoid recalculating vdom that hasn't changed. We'll explore this in the next chapter.

Let's continue to [Bonsai Guide Part 2: Dynamism](./02-dynamism.md).
