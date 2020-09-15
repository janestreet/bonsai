# Bonsai_web

In theory, Bonsai can be used to implement UI frameworks for a variety of
backends, but so far `Bonsai_web` is the only backend that has been built.

There are three main parts to the `Bonsai_web` library:

- An instantiation of the Bonsai functor
- Integration code for embedding Bonsai components in `Incr_dom` apps
- A method for starting Bonsai apps

# An instantiation of the Bonsai functor

When you `open Bonsai_web`, one of the modules that is now in scope is called
`Bonsai`.  This module is a direct re-export of the library also named
`bonsai`.

# Integration code for embedding Bonsai components in `Incr_dom` apps

Because Bonsai and `Incr_dom` are implemented similarly, Bonsai components can
be included in `Incr_dom` apps via the `Bonsai_web.To_incr_dom` module.  A more
detailed explanation can be found [here](./inside_incr_dom.md), with an example
of the embedding at `lib/bonsai_web/examples/inside_incr_dom`.

# A method for starting Bonsai apps

In Bonsai, the line between "application" and "component" is blurry.  Because
Bonsai does everything with `Bonsai.Value.t` and  `Bonsai.Computation.t`, the
component which contains all the other components in the application is
frequently called the "application component".

When you have a component that you want to put on a page, you'll want to
start using the functionality that is defined in the `Bonsai_web.Start`
module.

`Bonsai_web.Start.start` takes a `'a Bonsai.Computation.t` as a parameter to
the function, alongside a first-class module that describes how to pull a Vdom
node from that `'a` type.  The final parameter is
`bind_to_element_with_id:string` which will mount the component into a DOM
element in the browser which has an ID matching the provided value.
