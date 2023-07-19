# The History of Bonsai.t

This document chronicles the changes in the main `Bonsai.t` type through its
history.

## Motivation

Work on Bonsai started in late March 2019 after the conclusion of the first
Tools & Compilers teach-in.  The 2nd half of the teach-in was focused on
`Incr_dom`, and it gave us an opportunity to look over the shoulders
(literally) of people writing applications using that framework.  One of the
main frustrations from the assignments centered on the composability and
extensibility of `Incr_dom` programs, and the lack of a conceptual "component."

Most of the bugs that were encountered by students in the class were either due
to the application growing too big and becoming large and messy, or due to
mistakes being made while attempting to connect smaller more cleanly-separated
components.

But what is a "component"?  When most people use the word, they're talking
about a few behaviors.

1. Components should be composable.  Making larger components out
   of many smaller ones is paramount.
2. A component should be isolated from the rest of the application's code.  It
   shouldn't have its fingers in other components' business, nor should it
   expose too much of its own internal details.

## Bonsai Pre-History

The closest thing that `Incr_dom` has to a "component" as described above would
be the following type signature:

```ocaml
type ('model, 'action, 'state) component
  =  model: 'model Incr.t
  -> old_model: 'model option Incr.t
  -> inject: ('action -> Event.t)
  -> ('action, 'model, 'state) Incr_dom.Component.t Incr.t
```

This type signature (or something like it) is found in many places (though never
actually given a type), from `Incr_dom`'s own `App_intf.S`, to the Partial
Render Table's `Table.S.create`, to the internal organization of some
well-factored applications written at Jane Street.

Realizing that this type signature was the "real" component type (as opposed to
`Incr_dom.Component.t`), the first iteration of the `Bonsai.t` type looked
roughly like this:

```ocaml
module Bonsai : sig
  type ('model, 'action, 'state) t
    =  model: 'model Incr.t
    -> old_model: 'model option Incr.t
    -> inject: ('action -> Event.t)
    -> ('action, 'model, 'state) Incr_dom.Component.t Incr.t
end
```

## Bonsai Alpha

Pre-History Bonsai satisfies the "isolation" requirement of a component's
definition; hiding the types involved could be done manually with OCaml's
powerful abstraction primitives.  However, it was difficult to write code to
automatically compose these components.

A good proof-of-concept was a function that took two `Bonsai.t`s and smooshed
them together into a single `Bonsai.t`.  The signature looked something like
this:

```ocaml
val both
  :  ('model, 'action_1, 'state) t
  -> ('model, 'action_2, 'state) t
  -> ('model, ('action_1, 'action_2) Either.t, 'state) t
```

While writing an implementation of this may look feasible, there are some
details of the `Incr_dom.Component.t` type that made the composition
impossible.

### Infeasibility #1

The view that is contained inside of `Incr_dom.Component.t` has no natural
merge operation.  Given two `Vdom.Node.t` values, how do you produce a single
`Vdom.Node.t`?  Wrap it in a `<div>`?  There is no general, satisfying answer.

### Infeasibility #2

An instance of `('action, 'model, 'state) Incr_dom.Component.t` contains inside
of it a callback called `update_visibility` that returns `'model`, which is
called every time that `Incr_dom` detects that the visibility of an element on
the page may have changed.  `Incr_dom` expects that a super-component will
delegate these callbacks to its subcomponents, and they, in turn, will delegate
to their subcomponents.

The way that `Incr_dom` was able to deal with this is by offloading the "merge
two different updates to the same model" complexity off onto the developer.
Since Bonsai wants to perform composition automatically, this is not an
acceptable tradeoff.

### Solution: Drop <tt>Incr\_dom.Component.t</tt>

`Component.t` isn't cutting it, so let's make our own type, loosely modeled on it.
It will have fewer callbacks, and a generic return type.

```ocaml
module Snapshot : sig
  type ('model, 'action, 'result) t =
    { apply_action : schedule_event:(Event.t -> unit) -> 'action -> 'model
    ; result : 'result
    }
end

module Bonsai : sig
  type ('model, 'action, 'result) t
    =  model: 'model Incr.t
    -> old_model: 'model option Incr.t
    -> inject: ('action -> Event.t)
    -> ('model, 'action, 'result) Snapshot.t Incr.t

  val both
    :  ('model, 'action_1, 'result_1) t
    -> ('model, 'action_2, 'result_2) t
    -> ('model, ('action_1, 'action_2) Either.t, 'result_1 * 'result_2) t
end
```

The name "Snapshot" was chosen because it's how I described `Incr_dom`'s
`Component.t` type:

> An instance of `Incr_dom.Component.t` is a snapshot of the component
> at a particular instant in time.  Just look at the fields in the type,
> you've got `Vdom.Node.t`: the _current_ view, an `apply_action` callback
> which only describes the _current_ `apply_action`.  Indeed, it's the
> `Incr.t` in `Incr_dom.Component.t Incr.t` that gives the component its
> ability to change its view or behavior over time.

Gone are the `update_visibility` and `on_display` callbacks, and the "result"
of a component is generic instead of being restricted to `Vdom.Node.t`.

Now we can finally implement `both`!

Alongside another trivially implementable function `map`, Bonsai components now
have a let-syntax for trivial, multi-component composition.

```ocaml
module Bonsai : sig
  type ('model, 'action, 'result) t

  val both
    :  ('model, 'action_1, 'result_1) t
    -> ('model, 'action_2, 'result_2) t
    -> ('model, ('action_1, 'action_2) Either.t, 'result_1 * 'result_2) t

  val return : 'result -> (_, Nothing.t, 'result) t

  val map
    :  ('model, 'action, 'r1) t
    -> f:('r1 -> 'r2)
    -> ('model, 'action, 'r2) t

  module Let_syntax : sig
    ...
  end
end
```

Note: At this point, the concrete type of `Bonsai.t` stops being the `'model ->
'old_model -> etc...` function, and becomes a GADT which is hidden from the
user.  Conceptually, though, it remains the same as the function type seen
above.

## Bonsai Beta

After using this form of Bonsai in personal projects for a while, a certain
problem with the `Bonsai.t` signature kept cropping up: the explosion of
`'action` type parameters.  Every time that a component composition occurred
(either directly via `both`, or indirectly via the let-syntax), a new layer of
`Either.t` would show up on the resultant `'action` type parameter.
Infamously, a slide-show application built as a Bonsai demo had a top-level
component whose `'action` parameter was the following:

```ocaml
 (never_returns,
  (never_returns,
   (never_returns,
    (never_returns,
     (never_returns,
      ((Interaction.Action.t,
        (int Model_editor.Action.t, Code_slide.Action.t) Either.t)
       Either.t,
       ((Interaction.Action.t,
         (int Model_editor.Action.t, Code_slide.Action.t) Either.t)
        Either.t,
        (((Interaction.Action.t, Interaction.Action.t) Either.t,
          (Inner_model.t Model_editor.Action.t, Code_slide.Action.t) Either.t)
         Either.t,
         ((string * Interaction.Action.t,
           (int Core_kernel.String.Map.t Model_editor.Action.t,
            Code_slide.Action.t)
           Either.t)
          Either.t,
          ((never_returns, (never_returns, Code_slide.Action.t) Either.t)
           Either.t,
           ((never_returns, (never_returns, Code_slide.Action.t) Either.t)
            Either.t,
            ((never_returns, (never_returns, Code_slide.Action.t) Either.t)
             Either.t,
             ((Interaction.Action.t,
               (int Model_editor.Action.t, Code_slide.Action.t) Either.t)
              Either.t,
              (((Interaction.Action.t, Interaction.Action.t) Either.t,
                (Inner_model.t Model_editor.Action.t, Code_slide.Action.t)
                Either.t)
               Either.t,
               ((string * Interaction.Action.t,
                 (int Core_kernel.String.Map.t Model_editor.Action.t,
                  Code_slide.Action.t)
                 Either.t)
                Either.t,
                ((never_returns,
                  (never_returns, Code_slide.Action.t) Either.t)
                 Either.t,
                 (((string * Interaction.Action.t)
                   Bonsai_timetravel_example.Time_travel.Action.t,
                   (int Core_kernel.String.Map.t Model_editor.Action.t,
                    Code_slide.Action.t)
                   Either.t)
                  Either.t,
                  (never_returns, (never_returns, never_returns) Either.t)
                  Either.t)
                 Either.t)
                Either.t)
               Either.t)
              Either.t)
             Either.t)
            Either.t)
           Either.t)
          Either.t)
         Either.t)
        Either.t)
       Either.t)
      Either.t)
     Either.t)
    Either.t)
   Either.t)
  Either.t)
 Either.t
```

If you've ever wanted a binary tree of types, Bonsai Beta would gladly
provide.

Not only were the types unwieldy and hard to hide, they were also practically
worthless.  Raising another component's action -- while somewhat common in
`Incr_dom` apps -- is better accomplished by having one component pass its
`inject` function to another component.  Passing data from one component to
another is considerably easier in Bonsai than it was in `Incr_dom`, so
component-to-component communication by directly constructing the action type of
a neighboring component is prohibited.

This prompted us to write a feature hiding the type parameter from the public
API via some GADT type hackery, bringing the sig for Bonsai down to:

```ocaml
module Bonsai : sig
  type ('model, 'result) t

  val both
    :  ('model, 'result_1) t
    -> ('model, 'result_2) t
    -> ('model, 'result_1 * 'result_2) t

  val return : 'result -> (_, 'result) t

  val map
    :  ('model, 'r1) t
    -> f:('r1 -> 'r2)
    -> ('model, 'r2) t

  module Let_syntax : sig
    ...
  end
end
```

The removal of this type parameter was sorely needed too, because we'd been
wanting to add another type parameter, but felt like 4 was too much.

### Bonsai Release

The last type parameter is `'input`.  `'input` is for data that a component
needs to read, but doesn't require write access to.  Prior to `'input`,
users would either pass all their input into the component via `'model`
(accidentally making it writable in the process), or they'd close over
additional parameters like so:

```ocaml
let my_component something_to_close_over =
  ... construct a component here ...

```

However, this proved to be a nightmare:

1. Treating all of a component's input as mutable required storing everything
   that a component might want to read in the application's model.  This makes
   all input data mutable, and requires severe contortions in order to pass data
   from one component to another: the first component would have to write data
   into the model at a particular location that the second component is also
   looking at.  This is very fragile in practice.
2. Passing immutable output via closure made composition harder than
   we would have liked. For any data to flow to a component, the
   function calls constructing it need to be made during the initial
   construction of the app rather than at runtime.

For these reasons, a 3rd type was added to the `Bonsai` signature: `'input`, for
representing immutable input that the component would like to read but doesn't
need to write.

```ocaml
module Bonsai : sig
  type ('input, 'model, 'result) t

  val both
    :  ('input, 'model, 'result_1) t
    -> ('input, 'model, 'result_2) t
    -> ('input, 'model, 'result_1 * 'result_2) t

  val return : 'result -> (_, _, 'result) t

  val map
    :  ('input, 'model, 'r1) t
    -> f:('r1 -> 'r2)
    -> ('input, 'model, 'r2) t

  module Let_syntax : sig
    ...
  end
end
```

In addition to simplifying the uses of components, `Bonsai.t` gains a new
power: the ability to chain components together, piping the output of one
component into the input of another!

```ocaml
val ( >>> )
  :  ('input, 'model, 'result_1) t
  -> ('result_1, 'model, 'result_2) t
  -> ('input, 'model, 'result_2) t
```

with this operator, `Bonsai.t` is in the category of Arrows![^arrow]

This infix operator (found inside `Bonsai.Infix`) is wildly powerful and
provides the 2nd form of composability:

1. `Let_syntax` or `both`: Parallel composability
2. <strong>`>>>`: Sequential composability</strong>

And that brings us to the end of the `Bonsai.t` API journey!  The type
signature remains `('input, 'model, 'result) t` and although its implementation
will likely change drastically throughout the coming months and years, this
core type will stay the same (I hope).

### Model-less Bonsai

As if to spite me, mere weeks after writing that the type parameter would
stay the same, some of the London T&C team dared me to remove the model type
parameter.

At first I was skeptical, but after two days of prototyping, it was clear that
not only was the removal possible, but that it simultaneously made normal usage
of the library much easier while making most unidiomatic patterns impossible.

The feature jane/bonsai/modelectomy implements the removal of the type
parameter, changing the core type like so:

```diff
- ('input, 'model, 'result) Bonsai.t
+ ('input, 'result) Bonsai.t
```

To be clear, models are not going away.  Much like how actions still exist (but
don’t occupy a slot in the type), models become an implementation detail of a
component, instead of a core part of its API.

Removing `'model` had many effects, some more impactful than others.  The biggest
change is that components can no longer share models.  Much like how removing
the action type parameter removed the ability for components to send actions to
one another, removing the model type parameter prevents sharing of
component-internal state.  Fortunately, during the tree-smash that came along
with the change, I found that none of the apps or libraries that actually
relied on two components reading and writing to the same model. There were a
number of cases where one component performed reads and writes and another
component only performed reads, but that pattern is expressible with the
read+write component returning the model value as a result, passing it into the
second component via read-only input via component composition.

Another change is that a Bonsai.t value keeps track of its default model
internally.  In practice, this means that `Bonsai.of_module` and
`Bonsai.state_machine` have new, required, `default_model` parameters.  However,
the application developer no longer needs to recursively construct
`default_model` values for every sub-model in the application as they climb up
the component tree.  Composition via `let%map` or `>>>` automatically derives a
default model based on the default model of its constituent components.

Likely the most visible change is the complete removal of all of the
model projection functions.  No more `Bonsai.Model.field`, no more
`Bonsai.Model.f`, no more bugs where you copy and pasted a text box
component and forgot to change the model projection.

Finally, the types that can become models appear to be more restricted.  A
model must now be fully sexp-convertible and have a conservative equality
implementation.  For most models, these requirements aren't too onerous -
models should almost always contain nothing but plain data.  However, for
models that can't meet these criteria, it is acceptable for the `sexp_of`
function to be defined in terms of `sexp_of_opaque`, for the `of_sexp` function to
raise an exception, and for the equality function to default to physical
equality.  Together with the default model packed with each bonsai component,
combinator authors have access to a lot more power to introspect running Bonsai
apps, paving the way for powerful debugging tools.

[^arrow]: [Arrow: Haskell Wiki](https://wiki.haskell.org/Arrow)

### Leo's "Wormhole" proposal
#### ACT 1
##### Leo's Combinator Proposal: Wormhole

```ocaml
val wormhole
  : (read:(_, 'value) Bonsai.t -> ('input, 'r2) Bonsai.t)
  -> ('input * 'value, 'result) Packed.t
```

##### Implementation

During the construction of the incremental tree (what bonsai-devs call
"eval-time"), maintain an environment that maps the "read" Bonsai.t
back to the Incr.t that it is using.

In concrete: a `Univ_map` from `'a Type_equal.Id.t` to `'a Incr.t`.

    ../../src/env.ml


##### Benefits of wormhole: Ergonomics

Instead of threading some extraneous state through the bonsai graph via arrow
combinators, you can use this "read" component only in the places where you
care about the `'value` value.

```ocaml
wormhole (fun ~read ->
  let%map a = ...
  and     b = ...
  and     c = ...
  and     d = Bonsai.Map.assoc (module Int) (Bonsai.both (...) read)
)
```

##### Benefits of wormhole: Better incremental graph

The graph produced with wormhole is more direct!

```
o -- o -- o -- o
    /
o -`
```

```
o -- o -- o -- o
              /
o -----------`
```

##### Benefits of wormhole: Better semantics

This is confusing to some people:

```ocaml
let a: _ Bonsai.t = ... in
let%map b = a >>| f
and     c = a >>| g in
h b c

(both (Bonsai.input) a) >>> (wormhole (fun ~read ->
  let%map b = read >>| f
  and     c = read >>| g in
  h b c
))
```

But if 'a' is the `read` end of a wormhole, there's no duplication!

##### Bind-like

```ocaml
val bind_like
  :  ('input, 'r1) Bonsai.t
  -> f:((_, 'r1) Bonsai.t -> ('input, 'r2) Bonsai.t)
  -> ('input, 'r2) Bonsai.t


(both (Bonsai.input) a) >>> (wormhole (fun ~read ->
  let%map b = read >>| f
  and     c = read >>| g in
  h b c
))

bind_like a (fun read ->
  let%map b = read >>| f
  and     c = read >>| g in
  h b c
)
```

##### Real world usage

```
    extend_second paging
>>> extend_second sorting
>>> extend_second cell_focus
>>> extend_second tbody
>>> extend_second merge_key_handlers
>>> extend_second table
>>| make_result

old: ../../../../../../+share+/lib/bonsai_ui_components/src/table/table.ml
new : ../../../bonsai_ui_components/src/table/table.ml
```


#### ACT 2

```diff
 module Let_syntax : sig
   module Let_syntax : sig
+    val bind
+      :  ('input, 'r1) Bonsai.t
+      -> f:((_, 'r1) Bonsai.t -> ('input, 'r2) Bonsai.t)
+      -> ('input, 'r2) Bonsai.t
   end
 end
```

##### Pros

Huge ergonomics + performance win for basically all code that would currently
be implemented via the arrow combinators.

New API's are easy to write, like the following for writing a tabbed-ui interface:

```diff
-val tabs
-  :  (module Enum with type t = 'a)
-  -> specialize:
-       (  'a
-       -> ('input * ('a -> Vdom.Event.t), Vdom.node.t) Bonsai.t)
-  -> ('input, Vdom.Node.t) Bonsai.t

+val tabs
+  :  (module Enum with type t = 'a)
+  -> specialize:
+       (  set_tab:(_, 'a -> Event.t) Bonsai.t
+       -> 'a
+       -> ('input, Vdom.node.t) Bonsai.t)
+  -> ('input, Vdom.Node.t) Bonsai.t
```

Using that API

```ocaml
module My_tabs = struct
  type t = A | B | C [@@deriving sexp, enumerate]
end

tabs (module My_tabs) ~specialize:(fun set_tab -> function
  | A -> a_component
  | B -> b_component
  | C -> build_c_component set_tab)
```



##### Cons

It's not _really_ bind?  I wish we could use the new let-op
syntax in ocaml.

`and` doesn't work.  We'd need something like co-both for it to work?
Leo has a proposal here that I need to read more carefully.

No storing these vars!


#### ACT 3

##### Unit For All!

```diff
-val bind_like
-  :  ('input, 'r1) Bonsai.t
-  -> f:((_, 'r1) Bonsai.t -> ('input, 'r2) Bonsai.t)
-  -> ('input, 'r2) Bonsai.t

+val bind_like
+  :  (unit, 'r1) Bonsai.t
+  -> f:((unit, 'r1) Bonsai.t -> (unit, 'r2) Bonsai.t)
+  -> (unit, 'r2) Bonsai.t
```

##### Proc

```ocaml
val proc
  :  ((unit, 'input) Bonsai.t -> (unit, 'result) Bonsai.t)
  -> ('input, 'result) Bonsai.t

proc (fun input ->

  let%bind a = input
  let%bind b = apply some_component (let%map a = a and input = input in a, input)
)
```

##### Proc in use

```ocaml
let my_arrow = proc (fun i ->
  let%bind x = some_arrow <<< (i >>| a_field) in
  let%bind y = another_arrow <<<
    let%map x = x
    and     i = i in
    g x i
  in
  let%bind z = yet_another_arrow <<<
    let%map x = x
    and     y = y
    and     i = i in
    h x y i
  in
  z)
```
#### ACT 4

##### REMOVE BONSAI.T

It's pretty clear that regular `('input, 'result) Bonsai.t` and `(unit,
'result) Bonsai.t` are pretty different.  Specifically the semantics
of `map` being different on the "read" end of a bind, makes me think that
these should be different types.


```ocaml
module Fun : sig
  type ('input, 'result) t
end

module Val : sig
  type 'a t
end
```

##### Proc & other helpers

```ocaml
val proc : ('a Val.t -> 'b Val.t) -> ('a, 'b) Fun.t
val call : ('a, 'b) Fun.t -> 'a Val.t -> 'b Val.t
```

##### Syntax

Only define a let-syntax for Val.t

```
moduel Val : sig
  module Let_syntax : sig
    val sub: 'a Val.t -> f: ('a Val.t -> 'b Val.t) -> 'b Val.t
  end
end
```

##### DELETE ARROW

```diff
-module Arrow : sig
-  val first : ('input, 'result) t -> ('input * 'a, 'result * 'a) t
-  val second : ('input, 'result) t -> ('a * 'input, 'a * 'result) t
-  val split : ('i1, 'r1) t -> ('i2, 'r2) t -> ('i1 * 'i2, 'r1 * 'r2) t
-  val extend_first : ('input, 'result) t -> ('input, 'result * 'input) t
-  val extend_second : ('input, 'result) t -> ('input, 'input * 'result) t
-  val ( *** ) : ('i1, 'r1) t -> ('i2, 'r2) t -> ('i1 * 'i2, 'r1 * 'r2) t
-  val fanout : ('input, 'r1) t -> ('input, 'r2) t -> ('input, 'r1 * 'r2) t
-  val ( &&& ) : ('input, 'r1) t -> ('input, 'r2) t -> ('input, 'r1 * 'r2) t
-  val ( ^>> ) : ('i1 -> 'i2) -> ('i2, 'result) t -> ('i1, 'result) t
-  val ( >>^ ) : ('input, 'r1) t -> ('r1 -> 'r2) -> ('input, 'r2) t
-  val partial_compose_first
-    :  ('input, 'shared * 'output1) t
-    -> ('input * 'shared, 'output2) t
-    -> ('input, 'output1 * 'output2) t
-  val pipe
-    :  ('input, 'r1) t
-    -> into:('intermediate, 'r2) t
-    -> via:('input -> 'r1 -> 'intermediate)
-    -> finalize:('input -> 'r1 -> 'r2 -> 'r3)
-    -> ('input, 'r3) t
-end
```

##### Fin

```ocaml
module Fun : sig
  type ('a, 'b) t
  (* arrow*)
end

module Val : sig
  type 'a t (* = | Lookup 'a Type_equal.Id.t | Map of 'a t * 'a -> 'b | Both 'a t * 'b t  *)
  (* applicative *)

  (* make applicative syntax *)

  val map
    :  'a t
    -> f:('a -> 'b)
    -> 'b t

  val return: 'a -> 'a t
  val both : 'a t -> 'b t -> ('a * 'b) t
end

module Computation : sig
  type 'a t (* = (unit, 'a) Bonsai.t *)
end

val apply
  :  ('a, 'b) Fun.t
  -> 'a Val.t
  -> 'b Computation.t

val bind
  :  'a Computation.t
  -> f:('a Val.t -> 'b Computation.t)
  -> 'b Computation.t

val const: 'a -> 'a Computation.t

val return: 'a Val.t -> 'a Computation.t

val proc
  :  ('a Val.t -> 'b Computation.t)
  -> ('a, 'b) Fun.t


let syntax :
  sub - kinda like bind - defined on computation.t
  map -                 - defined on Val.t

val bind
  : 'a Val.t
  -> f:('a Val.t -> 'b Val.t)
  -> 'b Val.t

let call arrow value =
  let%bind result = value >>> arrow in
  result

  bind (value >>> arrow)  ~f:(fun r -> r)

let a = call arrow x
let


val bind
  :  ('a, 'b) Fun.t
  -> f:('b Val.t -> ('b, 'c) Fun.t)
  -> ('a, 'c) Fun.t

val read : 'a Val.t -> (_, 'a) Fun.t

type 'a computation
val call: ('a, 'b) Fun.t -> 'a Var.t -> 'a computation
val bind
  :  'a computation
  -> f:('a Val.t -> 'b computation)
  -> 'b computation

let a = call f x in
let%map a = a
and     b = b
in (a, b)
```

Bonsai is not a UI framework, it's a programming language

### Proc-like Bonsai and beyond

In early 2020, once again London T&C suggested a change to the API for the better similar to
Haskell's Arrow's "proc" notation that changed Bonsai's API. Please see
[proc.md](./proc.md) for the current (as of June 2022 at least) state of
Bonsai's API.

