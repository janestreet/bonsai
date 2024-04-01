# 01 - Virtual-DOM

Browser interfaces are described by a tree of HTML *elements*, each of
which can have some *attributes* attached. The `virtual_dom` library
provides an OCaml interface for constructing these trees.

In this chapter, we'll learn how to write HTML in OCaml using
`virtual_dom` and `ppx_css`.

## Vdom.Node.t

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
```{=html}
<aside>
```
For the example above and throughout the rest of this guide, try using
[inspect
element](https://developer.chrome.com/docs/devtools/open#inspect) on the
demo to see the generated HTML.
```{=html}
</aside>
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
[DOM node](https://developer.mozilla.org/en-US/docs/Web/HTML/Element).
If a constructor is missing, `Vdom.Node.create` is available to manually
specify the tag, but the `Virtual_dom` maintainers gladly accept
contributions back to the main library!
```{=html}
</aside>
```
## Vdom.Attr.t

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
Or color text with inline css:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=css -->
```
``` ocaml
let css : Vdom.Node.t =
  Vdom.Node.span ~attrs:[ [%css {|color: red;|}] ] [ Vdom.Node.text "this text is red" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#css">
```
```{=html}
</iframe>
```
```{=html}
<aside>
```
We have a how-to guide about [writing CSS in Bonsai](../how_to/css.mdx)!
```{=html}
</aside>
```
### Event Handlers

An important group of `Vdom.Attr.t`s register "event handlers" for user
interaction (like clicking on buttons or typing into a text box).

They usually receive a browser-level event value (which is almost always
ignored), alongside any useful data extracted from that event. For
example:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Vdom.Attr.on_click : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> Vdom.Attr.t
val Vdom.Attr.on_input : (Dom_html.event Js.t -> string -> unit Effect.t) -> Vdom.Attr.t
```

Here's how we can use `on_click`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/vdom_examples.ml,part=clicky_button -->
```
``` ocaml
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun (_evt : mouse_event) ->
          (* Alerts are generally bad UI; there's an `Effect.print_s` for logging *)
          Effect.alert "hello there!")
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
We'll learn about `Effect.t` --- our abstraction for side effects --- in
[chapter 2](./02-effects.mdx).

```{=html}
<aside>
```
Sometimes, you want to listen for events across your entire app. The
functions in `Virtual_dom.Global_listeners` provide `Attr.t`s that
attach listeners to the window while the attr is attached to some vdom
node that is currently on the page.
```{=html}
</aside>
```
## The Underlying Machinery

A virtual-DOM is an immutable tree of immutable data structures that
represents the view of the application at a point in time. This is in
contrast to [the DOM (Document Object
Model)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model),
which is a mutable tree of mutable UI elements.

```{=html}
<aside>
```
The term "virtual DOM" is not unique to Bonsai. Many other UI libraries
like [React](https://reactjs.org/docs/faq-internals.html),
[Elm](https://github.com/elm/virtual-DOM), and
[Vue](https://vuejs.org/v2/guide/render-function.html#The-Virtual-DOM)
use the "virtual DOM" approach for similar reasons.
```{=html}
</aside>
```
When we first compute our `Vdom.Node.t`, `virtual_dom` creates a
matching DOM tree in the browser. On further recomputations,
`virtual_dom` diffs the new virtual-DOM tree against its previous
version, and updates the DOM elements that have changed. Bonsai
schedules this diffing for you, so all you need to worry about is
producing your desired `Vdom.Node.t`.

```{=html}
<aside>
```
Virtual-DOM diffing does *not* check against the actual DOM; it assumes
that the DOM will not be changed underneath it.
```{=html}
</aside>
```
Creating virtual-DOM is much, much cheaper than real DOM, so only
modifying the DOM we need to is a big performance win. But since
virtual-DOM is immutable, doesn't that mean we need to create an entire
new tree every time we recalculate view? That seems scary, but because
Bonsai computes view *incrementally*, and shares work between
subcomputations, we can build pretty big and complicated web apps with
great performance.

### Diffing Lists

Diffing vdom produced from dynamic lists can be tricky. Because elements
can move around, be added, or removed, we need to re-diff the entire
list whenever anything in it changes. If we have big lists, this can be
expensive.

More concerningly, the virtual-DOM diffing algorithm won't associate
list elements with specific DOM nodes: if two elements in an input list
swap places, virtual-DOM will likely patch the two corresponding DOM
nodes to swap their content, instead of swapping the nodes themselves.
This can cause bugs if event listeners aren't properly moved around.

The `vdom_node_with_map_children` allows you to provide a
`Vdom.Node.t Map.t` instead of a `Vdom.Node.t list`, and will perform
efficient diffing and stable association of input elements to DOM nodes.
