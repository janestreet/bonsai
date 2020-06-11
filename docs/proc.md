# Bonsai.Proc An elegant API for a more civilized age

Bonsai is somewhat unique among Jane Street libraries for embracing the
[Arrow](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) as
its primary abstraction.  `('a, 'b) Bonsai.t` is conceptually very close
to a function `'a -> 'b`, but with two "superpowers" that regular OCaml
functions don't have:

1. Maintaining and composing state-machines for dynamic, interactive behavior
2. Building an incremental computation graph so that value recomputation happens
   as infrequently as possible.

Arrows occupy a comfy place [in between Applicatives and
Monads](http://homepages.inf.ed.ac.uk/slindley/papers/idioms-arrows-monads.pdf)
which is what gives Bonsai the ability to build a dependency graph between UI
components while still being static enough to permit automated composition of
the components state-machine-like models.

Unfortunately, the Arrow APIs can be a real struggle to work with.  With
functions like

```ocaml
val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
```

and

```ocaml
val both : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t`
```

`Bonsai.t` values are combined to form a new `Bonsai.t` with a different shape.

There are a number of issues with programming in this "higher-order" form:

- You can't give descriptive variable names to intermediate results
- "Tupling" many values together is required to pass more than one value
  through to other parts of the program
- The programmer can accidentally introduce duplication of work without
  intending to
- Many APIs are quite obtuse.  For example, `Bonsai.if_` requires that the
  condition, the `then_` branch, and the `else_` branch all have the same
  input.  This necessitates a product type containing all the values that might
  be needed in all three parameters

For the longest time, the Bonsai team thought that this was the best that we
could do, and we had plans to build a ppx ([like Haskell
did](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/arrow-notation.html))
to make Bonsai easier to work with.  Fortunately, through a series of
small breakthroughs, we found a new way to implement Bonsai, and with it, a new
API that is _much_ nicer to work with.  This work, largely inspired by the
[Arrow Calculus](http://homepages.inf.ed.ac.uk/slindley/papers/arrow-calculus.pdf)
is accompanied by an extension to `ppx_let` which we will cover later in this
document.


# `Bonsai.Proc`

The core Bonsai namespace remains unchanged (for now).  The new API is exposed
in a new module: `Bonsai.Proc`.  Here, we can find two types:

```ocaml
module Bonsai : sig
  module Proc: sig
    module Computation : sig
      type 'a t
    end

    module Value : sig
      type 'a t
      include Applicative.S with type 'a t := 'a t
    end
  end
end
```

In the place of `Bonsai.t` we have two types: `'a Computation.t` and `'a
Value.t`, which map onto the two Bonsai effects (state-machine composition, and
incremental value sharing respectively) described above.

## `let%sub`

Alongside the new Bonsai features, an extension to `ppx_let` was also developed
to make using Bonsai easier.  This extension - named `let%sub` - is intended to
be used with pairs of types that have a function that converts between them with
a function that looks very similar to bind.

Let's look at the three let-syntaxes and the type signatures for the functions
that they use.


**Applicative `map`**

```ocaml
val map : 'a t -> f:('a -> 'b) -> 'b t
val v : 'a t

let%map a = v in
  (* [a] has type ['a], and the result-expression
     is expected to be of type ['b] *)
```

**Monadic `bind`**

```ocaml
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val v : 'a t

let%bind a = Some "hello" in
  (* [a] has type ['a] and the result-expression
     is expected to be of type ['b t] *)
```

And finally, **Variable Substitution**

```ocaml
val sub : 'a t -> f:('a s -> 'b t) -> 'b t
val v : 'a t

let%sub a = v in
  (* [a] has type ['a s] and the result-expression
     is expected to be of type ['b t] *)
```

On the surface, `let%sub` looks an awful lot like `let%bind`, but the `~f`
function takes a value of `'a s` instead of `'a`.

In Bonsai, our `sub` function has this type signature:

```ocaml
val sub
  :  'a Computation.t
  -> f:('a Value.t -> 'b Computation.t)
  -> 'b Computation.t
```

## `Bonsai.Proc.Computation`

A value of type `'a Computation.t` represents the method for computing a value
of type `'a`.  This value may change during the lifetime of a program, and the
computation has an internal state machine that it can use to store state that
can change while the application is in use.

The same `'a Computation.t` can be used in multiple places in a program, and
these uses are independent, meaning that they _don't_ share their state, nor
does any work performed by one instance get reused by another.

In this normal OCaml code, if we see the same function being called multiple
times:

```ocaml
let a = f () in
let b = f () in
a + b
```

Then if `f` has side-effects (maybe printing to the console), then those
side-effects happen twice because `f` was called twice.

Similarly, if we wrote the code this way:

```ocaml
let a = f () in
let b = a in
a + b
```

you would (correctly) expect that the side-effect only happens once (when
computing `a`).  In these examples, the _code_ `f ()` is analogous to `_
Computation.t`.  If you want to have two separate values whose computations
maintain separate state, you would use two instances of `let%sub` to bind
them separately:

```ocaml
val some_computation : int Computation.t
val add : int Value.t -> int Value.t -> int Computation.t

let open Proc.Let_syntax in
let%sub a = some_computation in
let%sub b = some_computation in
add a b
```

Here, `a` and `b` can take on different values depending on the states of the
computations that produce them.

However, if you want to use just one value in multiple places, only use
`let%sub` once:

```ocaml
let open Proc.Let_syntax in
let%sub a = some_computation in
let     b = a in (* This is just aliasing [b] to [a] to mirror the above example *)
add a b
```

Here, `a` and `b` always take on the same value.

## `Bonsai.Proc.Value`

A value of type `'a Value.t` represents a value that may change during the
lifetime of the program.  For those familiar with the `Incremental` library,
this type is conceptually very similar to `Incr.t`.  The main method by which
you acquire values of type `'a Value.t` is by evaluating a `'a Computation.t`
via the `let%sub` syntax extension.

```ocaml
val c : 'a Computation.t

let%sub x = c in
  (* [x] has type ['a Value.t] here *)
```

In the example above, we run a computation `c` and store the result of that
computation in `x` which has type `Value.t`.

`Value.t` is an applicative, which means that you can combine multiple `Value`s
by using `Proc.Let_syntax`:

```ocaml
val a : int Value.t
val b : int Value.t

let (_ : int Value.t) =
  let open Proc.Let_syntax in
  let%map a = a
  and b = b in
  a + b
;;
```

## Other `Bonsai.Proc` functions

In addition to the new types, and the functions that convert between them, all
of the core Bonsai functions have been adapted.  Take `Bonsai.if_`; the pre-Proc
type signature is this:

```ocaml
val if_
  :  ('input -> bool)
  -> then_:('input, 'result) t
  -> else_:('input, 'result) t
  -> ('input, 'result) t
```

This signature is quite confusing.  The `then_` and `else_` branches are
conceptually functions that need to take the same `'input`.  You need to
extract a `bool` out of the `'input` for the condition as well?  A pro Bonsai
dev will know that you probably need to bundle up a whole bunch of data into
the `'input` value in order to make it useful for the condition _and_ both of
the branches, which will involve judicious use of `Bonsai.map_input` (AKA
`@>>`), to get these types to line up.  But it's certainly not obvious, and is
consistently a point of confusion for new users, and one of frustration for
experienced users.

Compare that to the new `Bonsai.Proc.if_`

```ocaml
val if_
  :  bool Value.t
  -> then_:'a Computation.t
  -> else_:'a Computation.t
  -> 'a Computation.t
```

This signature is better for two reasons:

1. It disassociates the condition, the `then_`-branch, and the `else_`-branch,
   you can compute these any way that you like, and compose them in this
   function without any bundling together of data for all three usages or any
   `map_input` nonsense.
2. The types involved make it clear which parts of the computation are
   evaluated eagerly (the condition, because `bool Value.t` requires the value
   to have already been computed) and which are evaluated on-demand (the
   `then_` and `else_` parameters, because `'a Computation.t` represents a
   value that _can_ be computed, but won't be unless necessary).

# `('a, 'b) Bonsai.t`?

Seeing as both the "Proc" API, and the regular "Arrow-based" API can exist in
the same code base at the same time, there must be some relationship between
the following types:

- `Bonsai.t`
- `Bonsai.Proc.Computation.t`
- `Bonsai.Proc.Value.t`

Indeed, going forward, `Bonsai.t` is defined as follows:

```ocaml
type ('a, 'b) t = 'a Proc.Value.t -> 'b Proc.Computation.t
                                  ^
                                   `-- (* Regular OCaml function! *)
```

This type equality is fundamental, and it is exposed in order to help you
transition your Bonsai apps to the new API without requiring a bunch of
conversion functions.


# Benefits of Proc?

We wouldn't have spent so much time writing the code (and this document) if it
wasn't for very good reasons.  Certainly, we could have kept the API the way it
was, and the (almost too smart) developers at Jane Street would have dealt with
it and continue to build awesome products in spite of its flaws.

However, after receiving feedback about some of the issues, and attempting to
write complex components ourselves, we believe that the benefits of the Proc
API are enormous, and that they will be the recommended way to build Bonsai
programs going forward.  The next few sections detail the individual ways that
the development experience is improved.

## Full Power of OCaml Functions

As explained above, the old `('a, 'b) Bonsai.t` is now nothing more than an
alias for `'a Value.t -> 'b Computation.t`.

The use of a regular OCaml function in that type signature implies several
brand new capabilities of Bonsai:

- 2 (and higher!) arity components
- 0-arity components
- Optional and labeled arguments
- Partial application of components

## Higher-arity components

In the old Bonsai world, we needed to stick all of the input that a component
needs into a single value in order to pass it through the `'input` type
parameter.  This frequently leads to a module that looks like `module Input :
type t = { ... ` in every single module that defines a Bonsai component.  With
Proc though, code that used to look like this:

```ocaml
(* Old Style *)

module Input : sig
  type t =
    { foo : int
    ; bar : string
    }
end

module Result : sig
  type t =
    { baz : float
    ; view : Vdom.Node.t
    }
end

val component : (Input.t, Result.t) Bonsai.t
```

Could now be expressed like this:

```ocaml
(* New Style *)

type t =
  { baz : float
  ; view : Vdom.Node.t
  }

val component
  :  foo:int Value.t
  -> bar:string Value.t
  -> t Computation.t
```

Using multiple arguments (when you want to) can make the code a lot cleaner,
and much of the code which merely packaged and unpackaged records is gone.

## Optional argument components

Along with higher-arity components, optional arguments are now expressible!
The author of a reusable component may write a type signature like this:

```ocaml
module Config : sig
  type t =
    { should_show_header : bool
    ; rows_to_display : int option
    ; allow_tabs : bool
    }
end

val component
  :  ?config:Config.t Value.t
  -> int list Value.t
  -> Vdom.Node.t Computation.t
```

Now, as the user of this component, you have three options available to you:

1. If the `Config.t` has defaults that you want to use, you can leave the optional
   argument unspecified.
2. However, if you want a different config, but one that stays the same for the
   entire lifetime of the program, you can pass in a constant `Value.t` (via
   `Value.return`.
2. However, if the configuration is dynamic (maybe the user of the application
   can interactively change parts of the config), then you can pass in a
   `Config.t Value.t` which is computed by other parts of the Bonsai program.

## 0-arity component

In pre-proc-bonsai, there was a common Bonsai type signature: `(unit,
something) Bonsai.t`.  The `unit` in input-parameter-position meant something
special; the component didn't have any input.  In `Bonsai.Proc` land, this is
completely avoided by having components that have a type signature only
including computation, like so:

```ocaml
type t =
  { text : string
  ; view : Vdom.Node.t
  }

val textbox : t Computation.t
```

No "input" or `Value.t` in sight.  The `Computation` is all that matters, so
it's all that's present in the signature.


## No weird duplication of work

This code can be very confusing to most people that have approached Bonsai.

```ocaml
val c : (input, result) Bonsai.t

let r =
  let%map a = c >>| foo
  and     b = c >>| bar in
  ...
```

Many would expect that, because `c` is used in two locations, that the
computation (and the state machine) done by `c` is reused.  However, this is
not the case!  Both uses of `c` are different components in the Bonsai graph,
and their internal states are not the same at runtime.

This behavior makes a lot of sense when you think about Bonsai components as
functions, like in the following OCaml code, where a function `f` is called
twice:

```ocaml
val f : input -> result

let r input =
  let a = foo (f input)
  and b = bar (f input) in
  ...
```

In the new `Proc` code, we no longer work with function-like arrows, so code
looks a lot more like typical OCaml where our preconceptions about when code is
executed will help us rather than hurt us.


## Easier-to-use APIs

As described above, `Bonsai.Proc.if_` has a much nicer API, but improvements
are found all throughout the `Bonsai.Proc` namespace.  Take the
`Bonsai.Map.assoc_*` family of functions, in the past, they were three very
similar looking functions

```ocaml
val assoc_input
  :  ('key, 'cmp) comparator
  -> ('data, 'result) t
  -> (('key, 'data, 'cmp) Map.t, ('key, 'result, 'cmp) Map.t) t
```

`assoc_input` took a `Bonsai.t` that only looked at the "data" of an element in
a map and produced a component that had an input with the type of the whole
map.

Meanwhile, its sibling `associ_input` accepted a component that took both the
"key" _and_ the "data" in tupled-form:

```ocaml
val associ_input
  :  ('key, 'cmp) comparator
  -> ('key * 'data, 'result) t
  -> (('key, 'data, 'cmp) Map.t, ('key, 'result, 'cmp) Map.t) t
```

And finally, we have `associ_input_with_extra`, a beast of an API that produces
a component that expects not only a map, but also an extra "input", which is
unrelated to the data in the map, but is passed to each internal component:

```ocaml
val associ_input_with_extra
  :  ('key, 'cmp) comparator
  -> ('key * 'data * 'input, 'result) t
  -> (('key, 'data, 'cmp) Map.t * 'input, ('key, 'result, 'cmp) Map.t) t
```

All three of these functions are replaced by `Bonsai.Proc.assoc`:

```ocaml
val assoc
  :  ('key, 'cmp) comparator
  -> ('key, 'data, 'cmp) Map.t Value.t
  -> f:('key Value.t -> 'data Value.t -> 'result Computation.t)
  -> ('key, 'result, 'cmp) Map.t Computation.t
```

Here, the `_ Map.t Value.t` that is being mapped over is passed into the
function explicitly, and the `f` function parameter exposes both the "key" and
"data" values which you can choose to use or ignore!  Furthermore, if the value
computed by `f` needs some extra data that isn't inside the map, you can just
close over them from the surrounding environment.

## No more arrow combinators

Likely the worst feature of the existing Bonsai API is the `Bonsai.Arrow`
module.  The functions found therein are the combinators which are used to
merge, combine, and compose `Bonsai.t` values.  To highlight a particularly
egregious example, suppose you had the following three bonsai components:

```ocaml
val year_picker:  (unit,             Vdom.Node.t * Year.t ) Bonsai.t
val month_picker: (Year.t,           Vdom.Node.t * Month.t) Bonsai.t
val day_picker:   (Year.t * Month.t, Vdom.Node.t * Day.t  ) Bonsai.t
```

and you wanted to compose a `Bonsai.t` that had type

```ocaml
val date_picker: (unit, Vdom.Node.t * Year.t * Month.t * Day.t) Bonsai.t
```

In the old world, using the Arrow combinators, you'd need to write something
like this:

```ocaml
open Bonsai.Arrow

let date_picker =
     year_picker
  >>| (fun (year_view, year) ->
        (year_view, year), year)
  |> second month_picker
  >>| (fun ((year_view, year), (month_view, month) ->
        (year_view, month_view, year, month), (year, month)
  |> second day_picker
  >>| (fun ((year_view, month_view, year, month), (day_view, day)) ->
        let view = Vdom.Node.div [] [year_view; month_view; day_view] in
        view, year, month, day)
```

The code above makes me sad in a way that I can't exactly put into a markdown
file.  I needed to store values for later in the left-hand-side of the tuple,
while forwarding values to the next component in the right-hand-side of that
tuple.

In `Bonsai.Proc` the above code would be written like this:

```ocaml
open Bonsai.Proc.Let_syntax

let date_picker =
  let%sub year_picker = year_picker in
  let year = Value.map year_picker ~f:(fun (_, year) -> year) in
  let%sub month_picker = month_picker year in
  let month = Value.map month_picker ~f:(fun (_, month) -> month) in
  let%sub day_picker = day_picker year month
  let day = Value.map day_picker ~f:(fun (_, day) -> day) in
  return (
    let%map year_view,  year  = year_picker
    and     month_view, month = month_picker
    and     day_view,   day   = day_picker in
    let view = Vdom.Node.div [] [year_view; month_view; day_view] in
    view, year, month, day)
```

You'll notice that the code isn't shorter, but it's considerably easier to
understand what's going on.  Furthermore, adding or removing code from this
function won't cause chains of type errors to propagate throughout the rest of
the code.

And it's going to get better too.  With some upcoming improvements to the
`let%sub` ppx, soon you'll be able to write code with pattern destructuring in
the let, allowing this to be written instead:

```ocaml
open Bonsai.Proc.Let_syntax

let date_picker =
  let%sub year_view,  year  = year_picker in
  let%sub month_view, month = month_picker year in
  let%sub day_view,   day   = day_picker year month
  return (
    let%map year_view  = year_view
    and     month_view = month_view
    and     day_view   = day_view
    and     year       = year
    and     month      = month
    and     day        = day in
    let view = Vdom.Node.div [] [year_view; month_view; day_view] in
    view, year, month, day)
```

## Better incremental graph

One huge downside of the arrow combinators is that it was frequently impossible
to build the best possible Incremental graph.  A graph like the one below

```
o -> b => c => d
 .---^
a
```

might have been required in order to pass `a` into `d` using the Arrow API.
But now, with `Proc`, the developer has much more flexibility in how to compose
the results of their computations, producing a more accurate - and faster -
graph:

```
o -> b -> c -> d
           .---^
          a
```

# Roll out Plan

`Bonsai.Proc` is available today!

While we've solved many of the issues that current Bonsai users deal with,
we don't want to force everyone to move to the new APIs.  However, it's also
unfortunate to have two APIs for Bonsai.  To accommodate these conflicting
interests, our compromise is to phase out the "old" Bonsai API gradually over
the course of the next year.

## Current Day (June 2020)

The new "Proc" types and APIs are added under a "Proc" submodule.  All existing
Bonsai code continues to build and function as it did before.

```ocaml
module Bonsai : sig
  type ('a, 'b) t

  (* Arrow APIs *)

  module Proc : sig
    module Computation : sig
      type 'a t
    end

    module Value : sig
      type 'a t
    end

    (* Proc APIs *)
  end
end
```

## 2 Months Out (August 2020)

The "Arrow"-based type and APIs are moved into a module called `Arrow`, and the
Proc types and APIs are promoted into the primary module.  This tree-smash will
be automated, and no application or library author will need to do any work to
keep their code building and running as it did before.

```ocaml
module Bonsai : sig
  module Computation : sig
    type 'a t
  end

  module Value : sig
    type 'a t
  end

  (* Proc APIs *)

  module Arrow : sig
    type ('a, 'b) t

    (* Arrow APIs *)
  end
end
```

## 6 Months Out (Dec 2020)

The "Arrow" submodule is renamed to `Arrow_deprecated`, but as before, no
application or library author will need to do anything.

```ocaml
module Bonsai : sig
  module Computation : sig
    type 'a t
  end

  module Value : sig
    type 'a t
  end

  (* Proc APIs *)

  module Arrow_deprecated : sig
    type ('a, 'b) t

    (* Arrow APIs *)
  end
end
```

## 12 Months Out (June 2021)

The `Arrow_deprecated` module is removed, and any apps or libraries using it
will need to be ported.  This translation will likely be done mechanically, and
it likely won't require any intervention, but I make no promises on the quality
of the code that is translated, so please don't make it come to this; I'm happy
to help you port things over manually if you approach the bonsai-dev team
earlier.

```ocaml
module Bonsai : sig
  module Computation : sig
    type 'a t
  end

  module Value : sig
    type 'a t
  end

  (* Proc APIs*)
end
```

# Conclusion

`Bonsai.Proc` is out!  Go use it!  And let us know what you think!

The Arrow is dead, long live the Arrow!
