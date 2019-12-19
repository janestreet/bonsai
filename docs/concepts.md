# Bonsai Concepts

<!--
```ocaml
# open! Bonsai_mdx
# open! Core_kernel
```
-->


## Function Composition
Functions have two primary modes of composition: sequential
composition and parallel composition.

Let's start with **sequential composition**, that is, taking the result of one
function and using it as the input to another function.  With the type
signatures `val f : a -> b` and `val g : b -> c` we can compose f into g,
producing another function of type `a -> c`.  In OCaml, this can be written a
few ways, including the following:

```ocaml
module type S = sig
  type a
  type b
  type c
  val f : a -> b
  val g : b -> c
end

module M(F: S) : sig
  val h : F.a -> F.c
end = struct
  include F
  let h a = g (f a)
  let h a = a |> f |> g
  let h = f >>> g
end
```

**Parallel composition** is when two functions are executed without any dependence
between them; the result of one function isn't needed in the other.  Given
functions f and g with slightly different type signatures `val f : a -> b`, `val g
: a -> c` we can build a new function that uses their results in aggregate; for
simplicity's sake, as a tuple:

```ocaml
module M(F: S) : sig
  val h : F.a -> F.b * F.c
end = struct
  include F
  let h a = f a, g a
end
```

While I realize that this section probably didn't blow anyone away, please keep
these concepts in mind.

## Functional UI

At first glance, functions appear to be a good building block for user
interfaces.  If you could imagine a hypothetical `View.t` type whose
value represents a specific display of UI elements, a GUI programmer
writing a function of type `val render : Application_state.t ->
View.t` should be enough to define the visualization of the state of
some app.  As the `Application_state` changes, this render function
gets called and pushes the newly generated View.t out to be displayed.

However, there are two main reasons that this style of “functional UI” is
rarely seen:

Render functions quickly become complex enough that advanced memoization
strategies are needed to prevent slowly recomputing an unchanged view.  UI
components require internal mutable state.  Consider the humble text box:
cursor position, selection ranges and scroll-state are all fundamental parts of
the user experience, but forcing the application developer to keep track of
that state for every text box in their application quickly devolves into chaos.

Amusingly, if you punt on just one of these concerns, Functional UI becomes
trivial to implement, but also deficient in some very serious ways.  Forcing
the application developer to track internal state for their components allows
them to use basic function memoization to avoid needless view recomputation.
Contrarily, if memoization is abandoned, internal state is easy: close over a
ref-cell with your state in it (but your view function is now likely quite
slow).  The presence of both internal state
and memoization appear to be in conflict.

# Enter the Arrow

Despite its issues, the pure-functional model is still very tempting, and if
we could annotate functions with an additional “state” type parameter, then a
library like Incremental could be used to reduce unnecessary recomputation.
Because OCaml doesn't support anything like this (and it's hard to imagine how
it could), we turn to a concept from category theory: the Arrow.  Arrows
describe data structures that behave like functions, but have something else
about them that make them unrepresentable as plain functions.  In the case of
Bonsai, we use our Arrow to provide both memoization and an internal
state-machine for our components.

The arrow type that Bonsai defines is called `Bonsai.t`, or written with its
type parameters, `('input, 'model, 'result) Bonsai.t`.  As you may have guessed
given the name, this type is the focal point of the library; every other
function in the library produced, manipulates, or composes values of this type.

A value of type `('input, 'model, 'result) Bonsai.t` can be thought of as a
function that goes from `'input` to `'result` with an internal state-machine of
type `'model`.  When computing the value of `'result`, a Bonsai component can
read its `'model` state-machine, and can return a `'result` that contains an
`Event.t` value which, when scheduled, is handed back to the component in
order to transition the state-machine.

## Pure Bonsai

Pure functions are still representable (and encouraged!) and will have the type
signature `('input, 'a, 'result) Bonsai.t`.  The generic `'a` type in the
`'model` type parameter location indicates that the state-machine could really
be anything: after all, the component doesn't read or write to the state
machine. Creating a pure `Bonsai.t` out of a pure function is as easy as
calling `Bonsai.pure`, which has the type signature:

```ocaml
val pure: f:(‘input -> ‘result) -> (‘input, _, ‘result) Bonsai.t
```

## Bonsai with State

Creating a Bonsai component with a state-machine is considerably more verbose
as it needs to encompass both the evaluation of the function and a
description of the behavior that transitions its internal state machine.

I'll leave out an example here, in favor of linking to the API documentation:
[`Bonsai.of_module`](./constructors.mdx)

## Composition

Just like functions, Bonsai.t can be composed both in parallel and serially.

A parallel composition is performed via the Bonsai let-syntax.  Because
Bonsai.t is an applicative, `let%map` is used to create a function out of many
smaller ones.

The bonsai composition

```ocaml
let h =
  let%map a = foo
  and b = bar
  and c = baz in
  a, b, c
```

Is roughly equivalent to the function composition

```ocaml
let h x =
  let a = foo x
  and b = bar x
  and c = baz x in
  a, b, c
```

Sequential composition is done by the `Bonsai.compose` function or the
`Bonsai.Infix.( >>> )` operator.

```ocaml
(* Bonsai *)
let h = f >>> g
let h = Bonsai.compose f g

(* OCaml function *)
let h a = g (f a)
```

## Caveat Compose

There is one huge asterisk that comes with both forms of Bonsai composition.
For parallel composition, the `'input` parameters for all the composed
`Bonsai.t` values must be the same.  For parallel and sequential composition,
the `'model` state machine type must also be the same.

On the surface, this seems like such a restrictive API that using
composition in practice would be impossible.  However, the rest of the
Bonsai library is there to help, with functions that transform the
`'input`, `'model` and `'result` type parameters in the `('input, 'model,
'result) Bonsai.t` type.

### Input Transformation

Transforming the input type on a component is as easy as passing a
transformation function to `Bonsai.map_input` or `Bonsai.Infix.( @>> )`.

```ocaml
let component : (string, _, _) Bonsai.t = string_printer
let component : (int,    _, _) Bonsai.t = Int.to_string @>> component
```

Using the input mapping functions, it's common to decorate every component in a
parallel composition with an input-conversion that passes on the necessary
parts of the super-component's input to the sub-component.

### Model Transformation

Transforming the `'model` type is considerably harder to conceptualize.  What
does it mean to change the type of a state-machine?  Stepping back even
further, what does it mean to compose the state-machines of multiple components
together?

To answer the first of those questions, it's possible to change the type of a
state-machine as long as the original state-machine is still accessible through
that new type.  That is to say, if you want to convert a Bonsai component from
`(_, ‘inner, _) Bonsai.t` to `(_, ‘outer, _) Bonsai.t`, a function with the
type `'outer -> ‘inner` is necessary.  However, when that component writes to
its `'inner` state-machine, we also need a way to get that value back out into
the `'outer` type.  This is done with a second function of type `'outer ->
‘inner -> ‘outer`.

As for the meaning of composing two state-machines together, typically this is
done by building a bigger state-machine that contains both of the
sub-state-machines side-by-side.

Both of these idioms are captured by the ever-helpful Fieldslib and
`[@@deriving fields]`.  Fieldslib provides a `Field.get` function which
implements the `'outer -> ‘inner` conversion function, and also `Field.fset`
which has the `'outer -> ‘inner -> ‘outer` function signature (and the
semantics we want).  Furthermore, building a large model with sub-models for
our sub components is frequently desirable.

To use the Fieldslib model projection functions, Bonsai has the
`Bonsai.Project.Model.field` function, which takes a Bonsai component alongside
a Fieldslib field, and produces a Bonsai component whose state machine has the
same type as the record that the field lives inside.

For examples of using the field projection (and input projections) read
[the chapter on projection](./projections.mdx).


## Writing a Bonsai App

An “application” in the Bonsai world is nothing more than a standard Bonsai
component that you have deemed large enough to consider an “app”.  To draw one
more comparison against functions, a “Bonsai app” is like the “main” function
(in a language that has one of those).
