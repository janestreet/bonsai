# Bonsai inside of `Incr_dom`

One of the main goals of Bonsai has always been good interoperability
with `Incr_dom`.  Because of the design decisions detailed in
[A History of Bonsai](./history.md), embedding a Bonsai component inside
of an `Incr_dom` app is trivial!  (Going the other way -- embedding part
of an `Incr_dom` program inside a Bonsai component -- is not yet possible).

The module that facilitates this translation is `Bonsai_web.To_incr_dom`, which
contains two functions (`convert` and `convert_with_extra`) and a module type
`S`.

Looking at the type signature for `convert`, we see that it's a function that takes a
Bonsai component as input and produces a first-class module of this `S` module-type
as output:

```ocaml
val convert
  :  ('input Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t)
  -> (module S
       with type Input.t = 'input
        and type Extra.t = unit)
```

So let's take a look at `S`:

```ocaml
module type S = sig
  module Input : T
  module Model : T
  module Extra : T

  module Action : sig
    type t [@@deriving sexp_of]
  end

  val create
    :  input:Input.t Incr.t
    -> old_model:Model.t option Incr.t
    -> model:Model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> (Action.t, Model.t, unit, Extra.t) Incr_dom.Component.with_extra Incr.t
end
```

If you've read the [History](./history.md) page, you'll recognize the `create`
function as what I defined as "the closest thing that `Incr_dom` has to
components."

It is common to use the `convert` function like so:

```ocaml
module My_bonsai_component : sig
  (* This is typically a standalone mli file *)
  include Bonsai_web.To_incr_dom.S
end = struct
  (* This is typically a standalone ml file *)
  include (val Bonsai_web.To_incr_dom.convert my_bonsai_component)
end
```

To see an example of these APIs in use, check out the example in
`../examples/inside_incr_dom/`.
