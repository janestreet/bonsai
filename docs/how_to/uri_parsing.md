# URI Parsing

`uri_parsing` is a library that helps generate an unambiguous set of
routes, and `parse_exn`/`unparse` functions for making a
[Url_var.t](./url_var.mdx), for any OCaml type.

Most routers take a list of strings / regexes describing routes, match
the URL string against them, and extract data/params into a map/list.

With `uri_parsing`, we start with an OCaml type, and define parsing
rules for it. The set of routes is generated automatically for us.

## Building a Parser

`Uri_parsing.Value_parser.*` provides utils for converting basic types
(`string`, `int`, `Time_ns.t`, sexpable types, etc) to/from strings.
There's also a `Value_parser.project`, which allows you to specify
custom conversion functions between types `'a` and `'b`.

To `'a Parser.t`, you need to specify *how* to parse (using a
`Value_parser.t`), and *where* to parse data from.

For instance, let's say we want to represent our navigation state as a
string, which we get from the path of the URL. Not practical, but here's
how we'd do it:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=string_parser -->
```
``` ocaml
let (parser : string Parser.t) = Parser.from_path Value_parser.string

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────┐
    │ All urls  │
    ├───────────┤
    │ /<string> │
    └───────────┘
    |}]
;;
```

Or maybe we only care about an int, and we want to get it from the query
params:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=int_parser -->
```
``` ocaml
let (parser : int Parser.t) = Parser.from_query_required ~key:"q" Value_parser.int

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────┐
    │ All urls  │
    ├───────────┤
    │ /?q=<int> │
    └───────────┘
    |}]
;;
```

Or maybe we want to use some abstract sexpable type (like `Id` from
before!), and we expect a list of them:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=many_id_parser -->
```
``` ocaml
let (parser : Id.t list Parser.t) =
  Parser.from_remaining_path (Value_parser.sexpable (module Id))
  |> Parser.with_prefix [ "With"; "a"; "Prefix" ]
;;

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────┐
    │ All urls                            │
    ├─────────────────────────────────────┤
    │ /With/a/Prefix/<multiple<sexpable>> │
    └─────────────────────────────────────┘
    |}]
;;
```

We also snuck in a `Parser.with_prefix`. This is one of several utils
that allows customizing how the generated routes will look. You can read
about all the `Parser.t` constructors and combinators in [the
mli](https://github.com/janestreet/bonsai/blob/master/uri_parsing/src/uri_parsing.mli).

None of these are remotely realistic URL structures. What we *really*
want is to parse some nested structure of variants/records, representing
pages/subpages and their data dependencies.

## Choosing a Type

A common app structure is a set of pages, where each page takes some
parameters. For instance, a forum might use the following type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
module Search_params = struct
  type t =
    { query : string
    ; author_id : Id.t option
    ; categories : Id.t list
    }
end

module Admin_page = struct
  type t =
    | Settings
    | Edit_user of Id.t
end

module User_page = struct
  module Subpage = struct
    type t =
      | Profile
      | Posts
  end

  type t = {
    user_id : Id.t;
    subpage : Subpage.t
  }
end

module Url = struct
  type t =
    | Homepage
    | Discussion of Id.t
    | Search of Search_params.t
    | User of User_page.t
    | Admin of Admin_page.t
end
```

Where `Id.t` is sexpable.

```{=html}
<aside>
```
We could have inlined `User_page.t` into `Url.t` by adding
`Profile of Id.t` and `Posts of Id.t`, but this makes use of `Id.t` less
ambiguous, and allows us to demonstrate nested records and variants.
Same goes for `Admin_page`.
```{=html}
</aside>
```
## Record = Set of Data

Let's start with `Search_params.t`. We can generate a `Parser.t` for a
record type by specifying a `Parser.t` for each field. We also need to
declare the order of all fields whos data comes from the path. [Typed
fields](https://github.com/janestreet/ppx_typed_fields) enforces that
we've gotten all cases, with the correct field types:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_search_params -->
```
``` ocaml
module Search_params = struct
  type t =
    { query : string
    ; author_id : Id.t option
    ; categories : Id.t list
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
    | Query -> Parser.from_path Value_parser.string
    | Author_id -> Parser.from_query_optional (Value_parser.sexpable (module Id))
    | Categories -> Parser.from_query_many (Value_parser.sexpable (module Id))
  ;;

  module Path_order = Parser.Record.Path_order (Typed_field)

  let path_order = Path_order.T [ Query ]
end

let search_params_parser = Parser.Record.make (module Search_params)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors search_params_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                 │
    ├──────────────────────────────────────────────────────────────────────────┤
    │ /<string>?author_id=<optional<sexpable>>&categories=<multiple<sexpable>> │
    └──────────────────────────────────────────────────────────────────────────┘
    |}]
;;
```

```{=html}
<aside>
```
We need to explicitly annotate `parser_for_field` with the [universally
quantified](https://v2.ocaml.org/manual/polymorphism.html#ss:explicit-polymorphism)
`type a. a Typed_field.t` because Typed Fields is implemented as a
[GADT](https://dev.realworldocaml.org/gadts.html).
```{=html}
</aside>
```
Query param keys are automatically derived from the record field name,
although they can be overriden by passing `~key` to
`Parser.from_query_*`.

## Variant = Choice of Route

Let's do the same with `Admin_page.t` to demonstrate `uri_parsing`'s
support for variants. It's nearly the same, except that we use
`typed_variants` instead of `typed_fields`, and don't need to specify a
`path_order`.

`Parser.Variant.make` will generate a separate route for each variant
case:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_admin_page -->
```
``` ocaml
module Admin_page = struct
  type t =
    | Settings
    | Edit_user of Id.t
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Settings -> Parser.unit
    | Edit_user -> Parser.from_path (Value_parser.sexpable (module Id))
  ;;
end

let admin_page_parser = Parser.Variant.make (module Admin_page)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors admin_page_parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────┐
    │ All urls              │
    ├───────────────────────┤
    │ /edit_user/<sexpable> │
    │ /settings             │
    └───────────────────────┘
    |}]
;;
```

`val : Parser.unit : Parser.t` is only really useful for variant cases
with no payload: it parses nothing.

By default, `uri_parsing` will use the lowercased variant names as
prefixes for the routes. You can override this with
`Parser.with_prefix`, `end_of_path`, or `with_remaining_path`.

## Nesting Variants and Records

Our `Url.t` above contains records nested in variants, and vice-versa.
`uri_parsing` is designed for this! Let's take a look at a parser for
`User_page`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_user_page -->
```
``` ocaml
module User_page = struct
  module Subpage = struct
    type t =
      | Profile
      | Posts
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Profile -> Parser.unit
      | Posts -> Parser.unit
    ;;
  end

  type t =
    { user_id : Id.t
    ; subpage : Subpage.t
    }
  [@@deriving typed_fields, sexp, equal]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
    | User_id -> Parser.from_path (Value_parser.sexpable (module Id))
    | Subpage -> Parser.Variant.make (module Subpage)
  ;;

  module Path_order = Parser.Record.Path_order (Typed_field)

  let path_order = Path_order.T [ User_id; Subpage ]
end

let user_page_parser = Parser.Record.make (module User_page)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors user_page_parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────┐
    │ All urls            │
    ├─────────────────────┤
    │ /<sexpable>/posts   │
    │ /<sexpable>/profile │
    └─────────────────────┘
    |}]
;;
```

```{=html}
<aside>
```
Note that we need to include `Subpage` in `path_order`, because its
parser matches against the path via the "profile" and "posts"
auto-generated prefixes.
```{=html}
</aside>
```
So nesting variants in records produces a separate route for each
combination of variants, where each route includes data from the
remaining record fields.

Let's take a look at "Variant of Record" and "Variant of Variant" by
writing a parser for `Url.t`, combining everything we've done so far:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_url -->
```
``` ocaml
module Url = struct
  type t =
    | Homepage
    | Discussion of Id.t
    | Search of Search_params.t
    | User of User_page.t
    | Admin of Admin_page.t
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.end_of_path Parser.unit
    | Discussion -> Parser.from_path (Value_parser.sexpable (module Id))
    | Search -> search_params_parser
    | User -> user_page_parser
    | Admin -> admin_page_parser
  ;;
end

let parser = Parser.Variant.make (module Url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /admin/edit_user/<sexpable>                                                              │
    │ /admin/settings                                                                          │
    │ /discussion/<sexpable>                                                                   │
    │ /search/<string>?search.author_id=<optional<sexpable>>&search.categories=<multiple<sexpa │
    │ ble>>                                                                                    │
    │ /user/<sexpable>/posts                                                                   │
    │ /user/<sexpable>/profile                                                                 │
    └──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
```

The last new parsing util we'll introduce for now is
`Parser.end_of_path`, which declares the end of a route. There's also a
`Parser.with_remaining_path`, which requires that the path segments
match some constant list of strings.

## Start With The Types

We said this towards the start of this guide, but `uri_parsing` is
type-first, not route first. You'll get the best results if you:

-   Design a type that best fits the navigational needs of your web UI
-   Build up a `Parser.t` for it, and get it to type-check and pass
    tests
-   Make small tweaks to the subparser definitions until the generated
    routes match what you want.

## Versioned Parsers

You probably won't get your navigational state type right the first
time. Let's say the first version of your forum looked like this:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_old -->
```
``` ocaml
module Old_url = struct
  type t =
    | Homepage
    | Post of Id.t
    | Search of string
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.end_of_path Parser.unit
    | Post -> Parser.from_path (Value_parser.sexpable (module Id))
    | Search -> Parser.from_path Value_parser.string
  ;;
end

let old_parser = Parser.Variant.make (module Old_url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors old_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────┐
    │ All urls         │
    ├──────────────────┤
    │ /                │
    │ /post/<sexpable> │
    │ /search/<string> │
    └──────────────────┘
    |}]
;;
```

`Versioned_parser.t` allows you to update to the new `Url.t` without
breaking old URLs:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/uri_parsing_examples.ml,part=forum_versioned -->
```
``` ocaml
let v1_parser = Versioned_parser.first_parser old_parser

let v2_parser =
  Versioned_parser.new_parser parser ~previous:v1_parser ~f:(function
    | Homepage -> Homepage
    | Post id -> Discussion id
    | Search query -> Search { query; author_id = None; categories = [] })
;;

let%expect_test _ =
  Versioned_parser.check_ok_and_print_urls_or_errors v2_parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /admin/edit_user/<sexpable>                                                              │
    │ /admin/settings                                                                          │
    │ /discussion/<sexpable>                                                                   │
    │ /search/<string>?search.author_id=<optional<sexpable>>&search.categories=<multiple<sexpa │
    │ ble>>                                                                                    │
    │ /user/<sexpable>/posts                                                                   │
    │ /user/<sexpable>/profile                                                                 │
    └──────────────────────────────────────────────────────────────────────────────────────────┘

           |
    falls back to
           |
           v

    URL parser looks good!
    ┌──────────────────┐
    │ All urls         │
    ├──────────────────┤
    │ /                │
    │ /post/<sexpable> │
    │ /search/<string> │
    └──────────────────┘
    |}]
;;
```

## Testing Your Parser

In every example, we've included an expect test that calls
`Parser.check_ok_and_print_urls_or_errors`. This will analyze your
`Parser.t` for any ambiguous / duplicated invalid cases, and raise an
error if there are issues. If everything is ok, it will print the
generated routes. It's also great for spotting regressions /
unintentional URL changes.

You should have such an inline expect test for your app's main parser,
and for any interesting subparsers you might want to debug.

## Path vs Query Params

`uri_parsing` can pull data from 2 [parts of the
URL](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/What_is_a_URL):

-   Path, e.g. `path/to/my/file`
-   Query, e.g. `?key1=value1&key2=value2&key3=value3`

The path is parsed as a `string list` by splitting on `/`. The query is
a `string list String.Map.t`, because every query key can correspond to
multiple string, e.g. `"?a=1,2,3"`. Some browsers also support
`"?a=1&a=2&a=3"`, but this is
[inconsistent](https://stackoverflow.com/questions/32572264/what-might-be-shufflling-my-query-string-parameters-constructed-in-javascript),
so `uri_parsing` does not support it.

When should you put data in the path vs. in the query? There's no right
and wrong answer, ultimately it's a matter of taste: a URL like
`website.com/profile/capybara_lover123` feels better than
`website.com?profile=capybara_lover123` and likewise `posts?after=dec-2`
is preferable to `posts/after/dec-2`.

A good rule of thumb is that paths should be used for things that are
"resource-like", and query-parameters be used for optional or
configurable parameters to that resource.
