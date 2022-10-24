# 03 - State

[Chapter 2](./02-dynamism.md) briefly touched on the fact that
computations capture internal state. This chapter takes a deeper look at
the primitives Bonsai provides for introducing and interacting with
local state.

# Simple State

The simplest kind of state is `Bonsai.state`. It returns both a value
tracking the state's current model, and also a function for updating
that model.

`ocaml skip val state   :  (module Model with type t = 'model)   -> default_model:'model   -> ('model * ('model -> unit Effect.t)) Computation.t`

This function has a few types that might be a bit confusing to a new
user of OCaml, so let's step through them one by one.

1.  `(module Model with type t = 'model)`: This is a first-class module
    that describes the type of the state. The type needs to be
    `sexp`able and `equal`able. Typically this argument is provided by
    wrapping an already existing module inside the
    first-class-module-packing syntax; for example, you might write
    `(module String)`.
2.  `default_model:'model`: This is the initial value contained in the
    state.

Let's break down a simple, yet realistic usage of this computation.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=textbox -->
```
``` ocaml
let textbox : (string * Vdom.Node.t) Computation.t =
  let%sub state, set_state = Bonsai.state (module String) ~default_model:"" in
  let%arr state = state
  and set_state = set_state in
  let view =
    Vdom.Node.input
      ~attr:Vdom.Attr.(value_prop state @ on_input (fun _ new_text -> set_state new_text))
      ()
  in
  state, view
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox">
```
```{=html}
</iframe>
```
The computation returns the current contents of a textbox, as well as
the textbox view itself. The view could be combined with the views from
other components, eventually becoming the view for the entire
application. The "current value" could be passed on to other components
(like we'll do later).

```{=html}
<aside>
```
In the Bonsai ecosystem, a function that takes any number of `Value.t`
as input and returns a `Computation.t` is considered a "component". (In
this example, "textbox" takes no `Value.t` as inputs, but zero is still
"any number", so `textbox` is a component.)
```{=html}
</aside>
```
`ocaml skip let%sub state, set_state = Bonsai.state (module String) ~default_model:"" in`

This line creates some string state initially containing the empty
string. We use `let%sub` to instantiate this state, giving us access to
`state` and `set_state`, which have types `string Value.t` and
`(string -> unit Effect.t) Value.t`, respectively.

The `let%arr` expression maps over two values to produce a computation
containing the string and the view. If we attempted to write this code
using `state` and `set_state` directly instead of through `let%arr`, the
resulting program would not type-check, since both of these variables
have `Value.t` types. `let%arr` is required in order to get access to
the data inside the values.

The actual construction of the textbox virtual-dom node is quite boring;
we add the `value_prop` property to keep the textbox contents in sync,
and also register an event handler for `on_input`, an event that fires
when the text in the textbox changes.

```{=html}
<aside>
```
It is not obvious why `value_prop` is needed. If the contents of the
textbox are populated by the user, why re-set them with the current
state's value?

The answer is complicated, but it boils down to the behavior of the
component in weirder circumstances than these basic examples, like

1.  The `set_state` function is called by something *other* than the
    textbox `on_input` function. In this scenario, you'd want the
    contents of the textbox to change alongside the `state`.
2.  The textbox component is removed from the page and then added back
    again. If `value_prop` wasn't there, the textbox's default state
    would be empty!

```{=html}
</aside>
```
When the event does fire, the `set_state` function is called with the
new string. `set_state` has type `string -> unit Effect.t`, which you
may recognize from the last section in the
[virtual-dom](./01-virtual_dom.md) chapter. This function is called with
the new textbox contents, and the event which is returned schedules the
state-setting in the Bonsai event queue.

This is the payoff for the unanswered questions in [the virtual-dom
Chapter](./01-virtual_dom.md):

1.  *How do I get values of type `unit Effect.t` that aren't just
    `Ignore` and `Many`*: State-transition functions returned by
    stateful Bonsai components will return `unit Effect.t`s.
2.  *Why would I want to use the Bonsai event queue anyway*: More
    complex stateful components (like `Bonsai.state_machine`) can
    witness the changes made to other stateful components, and the
    Bonsai event-queue guarantees that these updates occur in a
    consistent order and that downstream components witness changes made
    to upstream components.

# Multiple Textboxes

Now that we've built a single textbox component, let's use it in a
bigger component:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=two_textboxes -->
```
``` ocaml
let two_textboxes : Vdom.Node.t Computation.t =
  let%sub textbox_a = textbox in
  let%sub textbox_b = textbox in
  let%arr contents_a, view_a = textbox_a
  and contents_b, view_b = textbox_b in
  let display = Vdom.Node.textf "a: %s, b: %s" contents_a contents_b in
  Vdom.Node.div
    ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
    [ view_a; view_b; display ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#two_textboxes">
```
```{=html}
</iframe>
```
This code is structurally very similar to the textbox component from
earlier:

1.  It instantiates computations using `let%sub` (this time with the
    `textbox` component itself, rather than the primitive `Bonsai.state`
    computation).
2.  `let%arr` is used to build a computation by mapping over values
    previously bound by `let%sub`.

Of particular note is that the `textbox` component is instantiated twice
(using `let%sub`). Because of this, each textbox will have its own
independent state.

Just for kicks, it's easy to see what would happen if the computation is
evaluated once but used twice. In the following code, the only
difference between it and the previous example is this line:

``` diff
- let%sub textbox_b = textbox in
+ let textbox_b = textbox_a in
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=two_textboxes_shared_state -->
```
``` ocaml
let two_textboxes_shared_state : Vdom.Node.t Computation.t =
  let%sub textbox_a = textbox in
  let textbox_b = textbox_a in
  let%arr contents_a, view_a = textbox_a
  and contents_b, view_b = textbox_b in
  let display = Vdom.Node.textf "a: %s, b: %s" contents_a contents_b in
  Vdom.Node.div
    ~attr:(Vdom.Attr.style (Css_gen.display `Inline_grid))
    [ view_a; view_b; display ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#two_textboxes_shared_state">
```
```{=html}
</iframe>
```
Not very useful, but heartwarming that something sensible happens at
all.

# State Machine

While `Bonsai.state` is quite useful, sometimes the state contained
within an application more closely resembles a state-machine with
well-defined transitions between states.

Consider a "counter" component that stores (and displays) an integer,
alongside buttons which increment and decrement that integer. This
component can easily be implemented using `Bonsai.state`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter_state -->
```
``` ocaml
let state_based_counter : Vdom.Node.t Computation.t =
  let%sub state, set_state = Bonsai.state (module Int) ~default_model:0 in
  let%arr state = state
  and set_state = set_state in
  let decrement =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (fun _ -> set_state (state - 1)))
      [ Vdom.Node.text "-1" ]
  in
  let increment =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (fun _ -> set_state (state + 1)))
      [ Vdom.Node.text "+1" ]
  in
  Vdom.Node.div [ decrement; Vdom.Node.textf "%d" state; increment ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#state_based_counter">
```
```{=html}
</iframe>
```
But there's a tricky bug hidden in this implementation: if a user clicks
the button more than once within a span of 16-milliseconds, there's a
chance that both button clicks will set the same value! This is because
the "current state" value is closed over by the event handler, and this
value could be stale because the DOM (including event handlers) is only
updated once per frame (approx every 16ms).

```{=html}
<aside>
```
It's easy to say "clicking on a button that fast isn't particularly
likely", and that may be true, but users are *very fast* with their
keyboards, and using keyboard shortcuts to manipulate applications is
common. In addition, if the application has an exceptionally long frame,
then the user might accidentally click twice before the frame completes.
```{=html}
</aside>
```
Fortunately, `Bonsai.state_machine0` is here to help! It has this type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Bonsai.state_machine0
  :  (module Model with type t = 'model)
  -> (module Action with type t = 'action)
  -> default_model:'model
  -> apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> 'model
        -> 'action
        -> 'model)
  -> ('model * ('action -> unit Effect.t)) Computation.t
```

Compared to `Bonsai.state`, there are several similarities:

1.  A "Model" first-class-module is passed in, alongside the initial
    model value (`default_model`).
2.  The return value is a `Computation.t` that provides the current
    state alongside a function which schedules changes to the state.

The main difference is the additional `Action` module, and
`apply_action`. The apply-action parameter is a function with a fairly
long signature, but can be simplified down to the last section:
`'model -> 'action -> 'model`. This encodes the notion of a
state-machine transition: "given the current model and an action,
produce a new model."

```{=html}
<aside>
```
The `inject` and `schedule_event` parameters are rarely used. They are
only useful when a state-transition needs to schedule *another* state
transition, either for itself (by composing `inject` with
`schedule_event`) or for another state-machine (just calling
`schedule_event`).
```{=html}
</aside>
```
So how would we use `state_machine0` to fix the bug in the counter
application?

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/state_examples.ml,part=counter_state_machine -->
```
``` ocaml
module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp_of]
end

let counter_state_machine : Vdom.Node.t Computation.t =
  let%sub state, inject =
    Bonsai.state_machine0
      (module Int)
      (module Action)
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Increment -> model + 1
        | Decrement -> model - 1)
  in
  let%arr state = state
  and inject = inject in
  let decrement =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (fun _ -> inject Decrement))
      [ Vdom.Node.text "-1" ]
  in
  let increment =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (fun _ -> inject Increment))
      [ Vdom.Node.text "+1" ]
  in
  Vdom.Node.div [ decrement; Vdom.Node.textf "%d" state; increment ]
;;
```

First, an `Action` module is defined as a sum type that lists all the
operations that can be performed on the state-machine. This module is
passed in to the call to `state_machine0`. Then, the `apply_action`
function is defined as a model-transformation function.

Using the computation returned by `state_machine0` also changes: instead
of a "set-state" function, we get a function that "injects" our
`Action.t` into a `unit Effect.t`.

Now, when a button is clicked multiple times in quick succession,
instead of calling `set_state` multiple times with the same value,
Bonsai will call `inject` multiple times, and they'll be processed by
`apply_action` in order, producing the correct result.

```{=html}
<aside>
```
`state_machine0` has a "0" at the end to indicate that it takes "0"
additional inputs. Compare the type signatures between `state_machine0`
and `state_machine1`:

``` diff
-val state_machine0
+val state_machine1
   :  Source_code_position.t
   -> (module Model with type t = 'model)
   -> (module Action with type t = 'action)
   -> default_model:'model
   -> apply_action:
        (inject:('action -> unit Effect.t)
         -> schedule_event:(unit Effect.t -> unit)
+        -> 'input
         -> 'model
         -> 'action
         -> 'model)
+  -> 'input Value.t
   -> ('model * ('action -> unit Effect.t)) Computation.t
```

The input is provided to the state-machine in `'input Value.t` form, but
is available inside the `apply_action` as a `'input`. This allows the
state-transition function to depend on the results of other
computations.

There is no `state_machine2`, but implementing one would be trivial, by
tupling the input `Value.t`, and destructuring the inputs inside
`apply_action`.
```{=html}
</aside>
```
# Why should functional programmers be okay with stateful components?

UI programming is inherently stateful. Even a UI element as simple as a
textbox needs to keep some state around to store the current contents of
the textbox.

Sadly, many of the tools that functional programmers use for dealing
with state almost exclusively involve moving that state out of their
programs into a database, or by pulling mutable state out into a small
part of the program. These strategies can keep the majority of programs
relatively pure and easy to test, but sadly, they don't scale well to UI
components for a few reasons:

1.  The stateful nature is something that is desirable to test!
2.  Bonsai needs to know when the state for a component changes so that
    it can re-fire calculations of down-stream components.
3.  If every component were provided a state-getter and state-setter,
    this would make component composition more manual work for the
    programmer; as applications get bigger and bigger, the root element
    would need to manage potentially dozens or hundreds of states for
    each transitive sub-component.
4.  Adding or removing state from a component would be a breaking change
    for everyone using it.

One way to look at UI components is that they are portals through which
an application interacts with the messy world. The job of a component is
to translate the unpredictable user actions into a well-understood piece
of data.

Although the fact that components are stateful might injure your
functional programming dogmatism, in fact, it is quite in line with
functional programming principles, which aim to isolate effects. The
most common way to isolate effects is by having a small kernel of
effectful code invoke the pure majority of the logic; in other words, we
isolate state by shifting it toward the root of the program. Bonsai
offers an alternative tool for isolation. With Bonsai UI components,
effectful code gets wrapped up and managed so that the interface
provided by the component remains pure; in other words, we isolate state
by shifting it toward the leaves of the program.

On to [Chapter 4: Forms](./04-forms.md).
