# URL Parsing / Routing

Most web UIs use the URL to navigate between pages. This has a few
benefits over just storing "which page am I on" state:

1.  Users can save and share links to specific navigational states
    within the app
2.  The browser's forward and back buttons allow quick historical
    navigation
3.  Reading and editing URLs can be used (as a last resort maybe) for
    precise navigation

URLs are strings, but we usually want to use some OCaml data structure
to represent the navigational state. We'll need `parse_exn` and
`unparse` functions to convert between `Uri.t`s and our custom `t`.

It's critical that `parse_exn` and `unparse` "roundtrip":
`unparse (parse_exn (x))` must equal `x`, and `parse_exn (unparse (y))`
must equal `y`.

## Writing Parsers By Hand

Imagine you have a site with the following URLs:

-   `/search?q=capybara`
-   `/settings`

We could represent this as an OCaml type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/url_var_examples.ml,part=type -->
```
``` ocaml
  type t =
    | Homepage
    | Search of string
  [@@deriving sexp, equal]
```

Then, we need to write `parse_exn` and `unparse` functions. `Url_var`
will actually extract out the path and query from the URL into a
`Url_var.Components.t`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=url_var_components -->
```
``` ocaml
  module Components : sig
    type t =
      { path : string
      ; query : string list String.Map.t
      ; fragment : string option
      }
  end
```

So we need to write functions mapping between `Components.t` and our
custom `t`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/url_var_examples.ml,part=parse_unparse -->
```
``` ocaml
  let parse_exn ({ path; query; _ } : Url_var.Components.t) : t =
    let path = String.split path ~on:'/' in
    match path with
    | [ "home" ] -> Homepage
    | [ "search" ] ->
      (match Map.find (query : _ String.Map.t) "q" with
       | Some [ query ] -> Search query
       | None | Some [] -> failwith "search missing query param"
       | Some (_ :: _ :: _) -> failwith "search with too many query params")
    | _ -> failwith "unknown path"
  ;;

  let unparse (t : t) : Url_var.Components.t =
    match t with
    | Homepage -> Url_var.Components.create ~path:"home" ()
    | Search query ->
      Url_var.Components.create
        ~path:"search"
        ~query:(String.Map.singleton "q" [ query ])
        ()
  ;;
```

You'll want to write your own tests to verify that these functions
roundtrip correctly.

## Generating Parsers with `uri_parsing`

The [`uri_parsing` library](./uri_parsing.mdx) allows you to specify a
type, and provide rules for *how* it should be parsed. It generates
`parse_exn`/`unparse` functions that roundtrip, and expect test helper
functions that verify everything is unambiguous, and list the set of
routes.

You give up some control over what your routes will look like though.

## Creating a `Url_var.t`

The `bonsai_web_ui_url_var` library provides a `Url_var.t` type, which
is a global variable that stores the URL.

`Url_var.t`s can only be created in a web-browser. This means that if
you've factored your app out into a standard `lib/`, `test/` `bin/`
structure, then the `Url_var.Typed.make` function should be located in
`bin/`. If you stick it in the `lib/` section, then tests in `test/` may
fail because the url-var can't deal with being in an expect-test
environment.

### From a Hand-Written Parser

You can use `Url_var.create_exn`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=url_var_from_handwritten -->
```
``` ocaml
  module type T = sig
    type t [@@deriving sexp, equal]
  end

  module type S = sig
    include T

    val parse_exn : Components.t -> t
    val unparse : t -> Components.t
  end

  val create_exn : (module S with type t = 'a) -> fallback:'a -> 'a Url_var.t
```

### From a `Uri_parsing.Versioned_parser`

You can use `Url_var.Typed.make`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=url_var_from_uri_parsing -->
```
``` ocaml
  module Typed : sig
    val make
      :  ?on_fallback_raises:'a
      -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> (module T with type t = 'a)
      -> 'a Uri_parsing.Versioned_parser.t
      -> fallback:(Exn.t -> Components.t -> 'a)
      -> 'a Url_var.t
  end
```

## Using `Url_Var.t`

A `Url_var.t` is a mutable, global variable that provides a
Bonsai-friendly API for acccessing and modifying it:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=url_var_usage_api -->
```
``` ocaml
  val value : 'a Url_var.t -> 'a Bonsai.t
  val set_effect : ?how:[ `Push | `Replace ] -> 'a Url_var.t -> 'a -> unit Effect.t
```

Once you have your `'a Bonsai.t`, all you need to do is [`match%sub` on
it](../guide/05-control_flow.mdx), and you have a router!

Changing the `Url_var.t` will automatically update the [browser
history](https://developer.mozilla.org/en-US/docs/Web/API/History_API),
so that browser navigation controls work properly.

```{=html}
```
## Server Config Pitfalls

Before client-side navigation via `Url_var.t` can kick in, the web
server needs to handle your request. If it has its own routing, this can
cause problems. Since Bonsai does not currently support server-side
rendering, you might need to disable server-side routing.

If using `cohttp_static_handler`'s `Single_page_handler`, set
`` ~on_unknown_url:`Index ``:

    Single_page_handler.create_handler
       ~title:<title>
       ~assets:<assets>
       ~on_unknown_url:`Index

```{=html}
</aside>
```
