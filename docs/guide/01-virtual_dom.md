# 01 - Virtual-dom

Browser interfaces are described by a tree of HTML *elements*, each of
which can have some *attributes* attached. The `virtual_dom` library
provides an OCaml interface for constructing these trees.

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
The text node will frequently be the "leaf" of a view (there are no
"children" of a text node). Let's put some text inside a bulleted list
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
For the bulleted list, the `ul` and `li` functions are required. These
correspond to the [ul
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul)
and the [li
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li),
which MDN helpfully tells us stands for
`<u>`{=html}U`</u>`{=html}nordered `<u>`{=html}L`</u>`{=html}ist and
`<u>`{=html}L`</u>`{=html}ist `<u>`{=html}I`</u>`{=html}tem.

`h3` is short for "header level 3", and is responsible for the larger
font in the title text, and `div` is a ["content
division"](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div)
and serves as a useful wrapper for the rest of the content.

```{=html}
<aside>
```
There is a `Vdom.Node.*` node constructor function for *almost* every
[dom node](https://developer.mozilla.org/en-US/docs/Web/HTML/Element).
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
that they are attached to, for instance, by adding placeholder text to a
textbox:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=input_placeholder -->
```
``` ocaml
let input_placeholder : Vdom.Node.t =
  Vdom.Node.input ~attrs:[ Vdom.Attr.placeholder "placeholder text here" ] ()
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#input_placeholder">
```
```{=html}
</iframe>
```
Or coloring text with inline css:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=css_gen -->
```
``` ocaml
let css_gen : Vdom.Node.t =
  Vdom.Node.span
    ~attrs:[ Vdom.Attr.style (Css_gen.color (`Name "red")) ]
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
styling attributes.

Finally, there are "event handler" attributes which register functions
that are called when a user interacts with the element (like clicking on
buttons or typing into a text box).

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=clicky_button -->
```
``` ocaml
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun (_evt : mouse_event) ->
          alert "hello there!";
          Ui_effect.Ignore)
      ]
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

`ocaml skip val Vdom.Attr.on_click : (mouse_event -> unit Vdom.Effect.t) -> Vdom.Attr.t val Vdom.Attr.on_input : (input_event -> string -> unit Vdom.Effect.t) -> Vdom.Attr.t`

You'll notice that because `on_input` is used to respond to users typing
into a textbox, the handler function is also given a string that holds
the current contents of that textbox.

The browser-level event-values like `mouse_event` and `input_event` are
almost always ignored in Bonsai apps.

The return type for these event handler functions is
`unit Vdom.Effect.t`, which is the final type that we care about in the
`Virtual_dom` library.

# unit Vdom.Effect.t

In the example above, the `on_click` handler function returned
`Vdom.Effect.Ignore`. However, the alert definitely fires when you click
on it, so what is this value doing, and why must these event-handlers
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
With the (not-virtual) DOM, the program mutates the tree of UI
components in order to update the view, but with the virtual-DOM, the
program produces a new tree every time the view changes. While this may
appear to be a performance nightmare, many of the tools that we use to
reduce duplication of work in regular programs also work well to prevent
re-computing parts of this sub-view.

The `Virtual_dom` library also contains functions that diff two versions
of a virtual-dom tree. The diff can be used as instructions for mutating
the DOM to reflect the contents of the "next" virtual-DOM node. These
functions are quite fundamental, but Bonsai handles the calls to these
functions, so application developers are solely concerned with producing
new vdom trees.

Let's continue to [Bonsai Guide Part 2: Dynamism](./02-dynamism.md).
