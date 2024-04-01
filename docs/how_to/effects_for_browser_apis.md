# Effects for Browser APIs

The `Bonsai_web.Effect` module contains some effects providing
abstractions over browser APIs.

## Responding to DOM Events

The following are useful in event handlers:

-   `val Effect.Prevent_default: unit Effect.t`
-   `val Effect.Stop_propagation : unit Effect.t`
-   `val Effect.Stop_immediate_propagation : unit Effect.t`; if using
    this, you'll need to compose effects with
    `Effect.sequence_as_sibling`, not `Effect.all`.

If composing these with other effects, these **must** run first, because
if one of your other effects is asynchronous, these won't run in time to
modify the DOM event.

You must compose them with other effects using `Effect.Many`; you cannot
sequence effects after them using `let%bind`.
``{=html}

## Miscellaneous

There are also helper functions for some common DOM operations you might
want to do in an `Effect.t`:

-   `val Effect.print_s : Sexp.t -> unit Effect.t`
-   `val Effect.alert : string -> unit Effect.t`
-   `val Effect.reload_page : unit Effect.t`
-   `module Effect.Focus` contains some useful tools for focusing DOM
    elements via effect.
