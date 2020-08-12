# Bonsai for Incremental Users

The Incremental library is a great foundation for building web applications!
The notion that "small updates to the input should result in small (and quick!)
changes to the output" which Incremental is built upon is also the basis for
modern web application implementation: if the state of the application changes
a little bit, then the view of the UI is also likely to be very similar to the
previously computed view.  Indeed, you could mock up a very basic UI using a
function signature like the following:

```ocaml
val my_web_ui: Application_state.t Incr.t -> Vdom.Node.t Incr.t
```

Where `Application_state.t` describes the state of the entire application, and
a value of type `Vdom.Node.t`) contains all of the information necessary to
render the UI.

> "Vdom.Node.t" stands for "Virtual DOM Node", and "DOM" is an acronym for
> "Document Object Model".  While the DOM is an impure, mutable tree of UI
> components, the Virtual DOM is an immutable, pure representation, and we can
> convert from a Virtual DOM node to a DOM node quickly.

For very simple applications, the type signature above would actually work
fine!  It can efficiently compute a view for a given application-state, and
because Vdom.Node.t supports efficient diffing, updating the view on the page
can actually be quite quick as well.  It’s also quite obvious to see how you’d
build composable UI components with this type-signature as well.  If you had an
`Application_state` that was a record type:

```ocaml
type t = { field_a: Field_a.t ; field_b: Field_b.t }
```

Then you could quite trivially build `my_web_ui` using helper functions that had
signature like this:

```ocaml
val field_a_component: Field_a.t Incr.t -> Vdom.Node.t Incr.t
val field_b_component: Field_b.t Incr.t -> Vdom.Node.t Incr.t
```

However, there is one big snag: UI components frequently require internal
state.  Any GUI that has a simple checkbox element, at some level needs to
store the state "is this checkbox checked".  And if we look at the AP for our
`field_{a,b}_component` up above, it’s quite clear that there is no place to
store any extra state.  But maybe the subcomponent state could be stored in the
`Application_state`?  This would certainly work, (indeed stuffing all
subcomponent state inside a huge top-level data structure is a common pattern
in JavaScript React applications), but now the top-level of the application
would need to handle any-and-all updates to that state for all of its
subcomponents.

In Bonsai, the way that we solved this issue is to introduce a new type which
represents "computations that have state".  In addition to a type representing
"Incremental values", there’s another type that represents "incremental
computations".  These incremental computations are capable of maintaining an
incremental state machine without polluting its interface or forcing other
parts of the program to do the actual state maintenance for it.  Let’s look at
these types:

```ocaml
module Value : sig
  type 'a t include Applicative.S with type 'a t := 'a t
end

module Computation : sig
  type 'a t
end
```

While Bonsai uses Incremental internally, it does not expose the Incr.t type;
instead, Bonsai has `'a Value.t`, which is conceptually very similar to Incr.t
(but you may notice that it is only an Applicative, and not a Monad).

As its name suggests, a value of type `'a Computation.t` does not represent a
value changing over time (like an `Incr.t` or `Value.t`), but rather a
computation that can be evaluated to produce a Value.t

The function for evaluating a Computation.t and receiving a Value has this
signature:

```ocaml
val sub
  :  'a Computation.t
  -> f:('a Value.t -> 'b Computation.t)
  -> 'b Computation.t
```

Like most functions that have a similar-looking signature,
`Bonsai.Let_syntax.Let_syntax.sub` has a let-syntax associated with it.

```ocaml
let%sub y = (x : int Computation.t) in (* y has type [int Value.t] here *)
```

The whole point of `Computation.t` was to build components that had internal
state, so let’s make one!  This example will produce a Vdom.Node.t
Computation.t, and to reiterate, a value of that type can be evaluated in order
to produce a dynamic `Vdom.Node.t Value.t`.

```ocaml
open Bonsai.Let_syntax

let counter =
  let%sub state = Bonsai.state [%here] (module Int) ~default_model:0 in
  return @@
  let%map current_value, set_value = state in
  Vdom.Node.div []
    [ Vdom.Node.textf "%d" current_value
    ; Vdom.Node.button
      [ Vdom.Attr.on_click (fun _ -> set_value (current_value + 1)) ]
      [ Vdom.Node.text "increment" ]
    ; Vdom.Node.button
      [ Vdom.Attr.on_click (fun _ -> set_value (current_value - 1)) ]
      [ Vdom.Node.text "decrement" ]
    ]
;;
```

Now that we’ve defined `counter` (reminder, it has type `Vdom.Node.t
Computation.t`), we can run one of them in a web-page using the following:

```ocaml
open! Bonsai_web.Future

let _handle =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    counter
```

Or we could create two of them:

```ocaml
let two_counters =
  let%sub counter_1 = counter in
  let%sub counter_2 = counter in
  return @@
  let%map counter_1 = counter_1
  and counter_2 = counter_2 in
  Vdom.Node.div [] [ counter_1; counter_2 ]

let _handle =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    two_counters
```

Because the counter computation is evaluated twice (via `let%sub`), each
counter has its own state.
