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
interfaces.  If you could imagine a hypothetical `View.t` type whose value
represents a specific display of UI elements, a GUI programmer writing a
function of type `val render : Application_state.t -> View.t` should be enough
to define the visualization of the state of some app.  As the
`Application_state` changes, this render function gets called and causes the
newly generated View.t to be displayed.

However, there are two main reasons that this style of “functional UI” is
rarely seen:

Render functions quickly become complex enough that advanced memoization
strategies are needed to prevent spending a lot of time recomputing the whole
view for an application that is barely changing.

UI components require internal mutable state.  Consider the humble text box:
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

Despite its issues, the pure-functional model is still very tempting, and if we
could annotate OCaml functions with an additional “state” type parameter, then
a library like Incremental could be used to reduce unnecessary recomputation.
Because OCaml doesn't support this (and it's hard to imagine how it could), we
turn to a concept from category theory: the Arrow.  Arrows describe data
structures that behave like functions, but have something else about them that
make them unrepresentable as plain functions.  In the case of Bonsai, we use
our Arrow to provide both memoization and an internal state-machine for our
components.

The arrow type that Bonsai defines is called `Bonsai.t`, or written with its
type parameters, `('input, 'result) Bonsai.t`.  As you may have guessed
given the name, this type is the focal point of the library; every other
function in the library produces, manipulates, or composes values of this type.

A value of type `('input, 'result) Bonsai.t` can be thought of as a
function that goes from `'input` to `'result`.


## Pure Bonsai

Pure functions are still representable (and encouraged!) and will have the type
signature `('input, 'result) Bonsai.t`.  Creating a `Bonsai.t` out of a pure
function is as easy as calling `Bonsai.pure`, which has the type signature:

```ocaml
val pure: f:(‘input -> ‘result) -> (‘input, ‘result) Bonsai.t
```

## Bonsai with State

Creating a Bonsai component with a state-machine is considerably more verbose
as it needs to encompass both the evaluation of the function and a
description of the behavior that transitions its internal state machine.

I'll leave out an example here, in favor of pointing you to the API
documentation for `Bonsai.Proc.of_module1` in the file
`lib/bonsai/src/proc.mli`.

## Composition

Just like functions, Bonsai.t can be composed both in parallel and serially.

A parallel composition is performed via the Bonsai let-syntax.  Because
Bonsai.t is an applicative, `let%map` is used to create a large component out
of multiple smaller ones.

The bonsai composition:

```ocaml
let h =
  let%map a = foo
  and b = bar
  and c = baz in
  a, b, c
```

Is roughly equivalent to the plain OCaml function composition:

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

* For parallel composition, the `'input` parameters for all the composed
  `Bonsai.t` values must be the same.

On the surface, this seems like such a restrictive API that using
composition in practice would be impossible.  However, it's easy to
transform a Bonsai component's input by "un-mapping" it with a
function.

### Input Transformation

Transforming the input type on a component is as easy as passing a
transformation function to `Bonsai.map_input` or `Bonsai.Infix.( @>> )`.

```ocaml
(* @>> changes the 'input in ('input, 'result) Bonsai.t *)
let component : (string, _) Bonsai.t = string_printer
let component : (int,    _) Bonsai.t = Int.to_string @>> component
```

Using the input mapping functions, it's common to decorate every component in a
parallel composition with an input-conversion that passes on the necessary
parts of the super-component's input to the sub-component like so:

```ocaml
let supercomponent =
  let%map a = convert_a @>> component_a
  and     b = convert_b @>> component_b
  and     c = convert_c @>> component_c
  in a, b, c
```

## Writing a Bonsai App

An “application” in the Bonsai world is nothing more than a standard Bonsai
component that you have deemed large enough to consider an “app”.  To draw one
more comparison against functions, a “Bonsai app” is like the “main” function
(in a language that has one of those).
