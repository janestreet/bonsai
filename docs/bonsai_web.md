# Bonsai_web

In theory, Bonsai can be used to implement UI frameworks for a variety of
backends, but so far `Bonsai_web` is the only backend that has been built.

There are three main parts to the `Bonsai_web` library:

- An instantiation of the Bonsai functor
- Integration code for embedding Bonsai components in `Incr_dom` apps
- A method for starting Bonsai apps

# An instantiation of the Bonsai functor

When you `open Bonsai_web`, one of the modules that is now in scope is called
`Bonsai`.  This module is _not_ a direct re-export of the library also named
`bonsai` because the main Bonsai library is exclusively a functor named `Make`.
Calling the `Make` functor and re-exporting the result is the job of
`Bonsai_web`, and it re-exports the resultant module as `Bonsai_web.Bonsai`.

# Integration code for embedding Bonsai components in `Incr_dom` apps

Because Bonsai and `Incr_dom` are implemented similarly, Bonsai components can
be included in `Incr_dom` apps via the `Bonsai_web.To_incr_dom` module.  A more
detailed explaination can be found [here](./inside_incr_dom.md), with an example
of the embedding at `lib/bonsai_web/examples/inside_incr_dom`.

# A method for starting Bonsai apps

In Bonsai, the line between "application" and "component" is blurry.  Because
Bonsai does everything with `Bonsai.t` values, the component which contains all
the other components in the application is frequently called the "application
component".

When you have a component that you want to put on a page, you'll want to
start using the functionality that is defined in the `Bonsai_web.Start`
module.

Both `Bonsai_web.Start.start` and `Bonsai_web.Start.start_standalone` take
a Bonsai component as a parameter to the function, alongside the initial
input that the component initially reads from.  The final parameter is
`bind_to_element_with_id:string` which will mount the component into a DOM
element in the browser which has an ID matching the provided value.

Where `Bonsai_web.Start.start` and `Bonsai_web.Start.start_standalone` differ
is in the type of Bonsai component that they expect, as well as the type of
application handle (more on that later) that is returned.

## <tt> Bonsai\_web.Start.start </tt>

This is the main entrypoint for most Bonsai apps.  It is built for apps that
communicate with the outside world (like networked RPCs), and as a result of
this requirement, the Bonsai component that it starts needs a way to both send
actions to the outside, as well as a way to receive actions from the outside.

These requirements are encoded in the types of the `'input` and `'result`
type parameters of the Bonsai component that `start` is passed:

```
(('input, 'outgoing) App_input.t, 'incoming App_result.t) Bonsai.t
```

### `App_input.t`

`App_input.t` can be simplified to this type:

```
type ('input, 'outgoing) t = 'input * ('outgoing -> Event.t)
```

By extending the input to this component with a function that produces events,
the Bonsai component can send actions of type `'outgoing` to the outside
[handle](#bonsai_web.start.handle.t).


### `App_result.t`
`App_result.t` can be simplified to this type:

```
type 'incoming t = Vdom.Node.t * ('incoming -> Event.t)
```

By extending the view with a function that produces events the Bonsai
component can receive actions from the [handle](#bonsai_web.start.handle.t).

## <tt> Bonsai\_web.Start.start\_standalone </tt>

The purpose of `start_standalone` is to start an application that _does not_
need to communicate with the outside world.  Because of this, the API is fairly
straightforward.  The Bonsai component parameter has an easily understandable
type: `('input, Vdom.Node.t) Bonsai.t`, and it returns a
`('input, Nothing.t, Nothing.t) Bonsai_web.Start.Handle.t`.

## <tt> Bonsai\_web.Start.Handle.t </tt>

The value that both `start` and `start_standalone` return is an instance of
`('input, 'incoming, 'outgoing) Start.Handle.t` with `'incoming` and
`'outgoing` type parameters set to their respective types if `Start.start` was
called, or `Nothing.t` and `Nothing.t` if `Start.start_standalone` was used.

The functions available to call on this handle type are important, but they're
heavily documented in `lib/bonsai_web/src/start.mli`, so I won't bother
repeating the docs here.
