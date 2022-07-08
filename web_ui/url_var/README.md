# Url Parsing / Routing
URL Parsing / Routing
---------------------

For any web application that supports navigation between different pages, URLs can be an
easy way to dramatically improve the user experience: they automatically give your app
some very useful features

1. Users can save and share links to specific navigational states within the app
2. The browsers forward and back buttons allow quick historical navigation
3. Reading and editing URLs can be used (as a last resort maybe) for precise navigation

It might not be obvious at first, but URL integration is bidirectional:

- when the URL changes, the application responds by updating its navigation
- when the user navigates in the app, the URL should be updated to match

In Bonsai, the `'a Url_var.t` type is used to manage this synchronization; when given a
parsing function (that parses a URL into values of your domain-specific `'a` type) and an
"unparsing" function (which turns `'a` back into a URL), the url-var can be treated like a
[`Var.t` and it'll handle all the browser-specific url management for you!

In this guide, we'll start by building a url-var by implementing parse and unparse by
hand, but the rest of this chapter will focus on the API built for the typed-fields ppx,
which will simultaneously generate the parser _and_ unparser functions for you (and ensure
that the printer and parser round-trip)!

# Handwritten Parse / Unparse

Imagine you have a site with the following URLs:

- `/search?q=capybara`
- `/settings`

URL Var's previous API allowed you to parse/unparse this URL by you manually implementing
its parse and unparse functions:

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=old_api -->
```ocaml
  module My_google_clone = struct
    type t =
      | Homepage
      | Search of string
    [@@deriving sexp, equal]

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
  end
```

You would have needed to write your own tests in order verify that your parser/unparser
successfully round-trip (`parse_exn(unparse(x))` gives you back the original `x`).

# Typed field Parse / Unparse

With URL Var's typed field API, you can define a module that is able to parse a URL into
`My_google_clone.t` and unparse `My_google_clone.t` into a URL:

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=search_example -->
```ocaml
module My_google_clone = struct
  type t =
    | Homepage
    | Search of string
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.unit
    | Search -> Parser.from_query_required ~key:"q" Value_parser.string
  ;;
end

let parser = Parser.Variant.make (module My_google_clone)
let versioned_parser = Versioned_parser.first_parser parser

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────┐
    │ All urls           │
    ├────────────────────┤
    │ /homepage          │
    │ /search?q=<string> │
    └────────────────────┘ |}]
;;
```


After creating a `My_google_clone.t Url_var.t` like this:

```
let url_var =
  Url_var.Typed.make
    (module My_google_clone)
    versioned_parser
    ~fallback:(fun _exn _components -> My_google_clone.Whoops)
;;
```

You can then get an automatically-updating `My_google_clone.t Value.t` with
`Url_var.value` which updates any time that the url changes and can be threaded into the
Bonsai computation like any other `Value.t`.

On the other side, `Url_var.set_effect` can be called to to change the content of the
url-var, and in doing so, will update the URL to reflect it.

The rest of the operations for `Url_var.t` are intended to be used outside of a
bonsai app:

```
type 'a t

(* for use in bonsai computation *)
val value : 'a t -> 'a Value.t
val set_effect : 'a t -> 'a -> unit Effect.t

(* for use outside of bonsai computation *)
val update : 'a t -> f:('a -> 'a) -> unit
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
```

<aside>
The actual call to `Url_var.Typed.make` must only run in a web-browser.  This means that
if you've factored your app out into a standard `lib/`, `test/` `bin/` structure, then the
`Url_var.Typed.make` function should be located in `bin/`.  If you stick it in the
`lib/` section, then tests in `test/` may fail because the url-var can't deal with being
in an expect-test environment.
</aside>

<aside>
If you are using cohttp_static_handler's Single_page_handler to serve your page,
it will redirect unknown urls on its own which prevents url_var from doing
any parsing/error handling. You can get around this by setting the
`on_unknown_url` parameter to `` `Index``

```
Single_page_handler.create_handler
   ~title:<title>
   ~assets:<assets>
   ~on_unknown_url:`Index
```
</aside>

## What is a path and what is a query?

URLs have lots of different
[parts](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/What_is_a_URL):

However, URL Var only focuses on two of them, the path and the query.

Given `example.com/path/to/my/file?key1=value1&key2=value2&key3=value3` has the following
parts:
- Path `path/to/my/file`
- Query `?key1=value1&key2=value2&key3=value3`

In the eyes of URL Var, a path is a list of strings separated by `/`, and a query is a
key-value pair list separated by `&`. The functions in the Typed API make heavy
use of this terminology.

The `path` and `query` together are represented like this:

```
module Components = struct
  type t =
    { path : string list
    ; query : string list String.Map.t
    }
end
```

Something that's weird about the above representation is that the query is a string-to-string-list pairing
instead of a string-to-string pairing. The reason for this is weird. What should `"?a=1&a=2&a=3"` parse to?
Some browsers parse this to `{a : [1, 2, 3]}`, others drop all of the duplicate instances except one,
others even [shuffle the keys as an anti-tracking measure](https://stackoverflow.com/questions/32572264/what-might-be-shufflling-my-query-string-parameters-constructed-in-javascript).
URL Var disallows repeated keys. When you use `from_query_many` the URL shape it expects is `"?a=1,2,3"` instead of
`"?a=1&a=2&a=3"`.

<aside>
When should you put something in the path vs. in the query? There's no right and wrong
answer, ultimately it's a matter of taste: a URL like
`website.com/profile/capybara_lover123` feels better than
`website.com?profile=capybara_lover123` and likewise `posts?after=dec-2` is preferable to
`posts/after/dec-2`.

A good rule of thumb is that paths should be used for things that are "resource-like", and
query-parameters be used for optional or configurable parameters to that resource.
</aside>


## Records

If you want to parse/unparse your URL or a part of your URL as a record you can use the
`Parser.Record` module which needs you to derive `typed_fields` on your record type.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=record_all_from_query -->
```ocaml
  module My_google_clone = struct
    type t =
      { query : string
      ; language : string
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language -> Parser.from_query_required Value_parser.string
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────┐
      │ All urls                       │
      ├────────────────────────────────┤
      │ /?language=<string>&q=<string> │
      └────────────────────────────────┘ |}]
  ;;
```


The key of `Query` is `"q"` since it's explicitly given through the `key` optional
parameter. The key of `Language` is `"language"` which was inferred with the help of
`typed_fields` since no `key` was given.

`path_order` is not really doing anything right now, but will be useful shortly.

<aside>
The `: type a. a Typed_field.t -> a Parser.t` type annotation might be confusing if it's
the first time you see it, but the good and bad thing is that it's always the same
annotation. The reason that this annotation is needed is because `'a Typed_field.t` is a
GADT, which results in each of its constructors having different types which can result in
weird type errors without the explicit annotation. There are cool articles and resources
that go over this topic in more detail, but for now, it's totally fine to just always type
the same type annotation `type a. a Typed_field.t -> a Parser.t`.
</aside>

It might be good to have [language] have its own type. We can use `Parser.project` or
`Value_parser.project` to change their types.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=record_2 -->
```ocaml
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_query_required
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────┐
      │ All urls                                │
      ├─────────────────────────────────────────┤
      │ /?language=<project<string>>&q=<string> │
      └─────────────────────────────────────────┘ |}]
  ;;
```

<aside>
We could've also used `Value_parser.stringable (module Language)` as a shorthand for
projecting stringable types. On a similar note, `Value_parser` also provides other
primitives like `sexpable` and `binable_via_base64`, `int`, `float`, and others.
</aside>

Right now, we are taking both fields the query, but you might want to design your URL such
that users see `google.com/en/?q=capybara` instead of
`google.com/?language=en&q=capybara`. Instead of `from_query_required`, we can use
`from_path`.


<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=record_path_error -->
```ocaml
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_path
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      Error with parser.
      ┌────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────┐
      │ Check name             │ Error message                                                                         │
      ├────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────┤
      │ Sane path orders check │ ("Each path parser must be present in path order. The following fields were missing:" │
      │                        │  (missing_fields (Language)))                                                         │
      └────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────┘ |}]
  ;;
```

We get an error! The reason for this is that, unlike query fields where keys can be parsed
and unparsed in any order since it's a `string list String.Map.t` under the hood, the path
is just a `string list` where the order in which you parse things matters. A URL like
`google.com/search/en` is different from `google.com/en/search`. If you take multiple
fields from the path, then you need to specify the order in which they should be parsed
and unparsed in. `check_ok_and_print_urls_or_errors` will let you know about any issues
statically. Here the problem can be fixed by adding `Language` to `path_order`.


<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=record_path -->
```ocaml
  module My_google_clone = struct
    module Language : sig
      type t [@@deriving sexp, equal]

      val to_string : t -> string
      val of_string : string -> t
    end = struct
      include String
    end

    type t =
      { query : string
      ; language : Language.t
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Query -> Parser.from_query_required ~key:"q" Value_parser.string
      | Language ->
        Parser.from_path
          (Value_parser.project
             Value_parser.string
             ~parse_exn:Language.of_string
             ~unparse:Language.to_string)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T [ Language ]
  end

  let parser = Parser.Record.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────────────────────────┐
      │ All urls                      │
      ├───────────────────────────────┤
      │ /<project<string>>?q=<string> │
      └───────────────────────────────┘ |}]
  ;;
```

Finally, although it's not ergonomic in this example, it's worth mentioning what happens
when you nest records.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=record_path_3 -->
```ocaml
  module Nested = struct
    type t =
      { foo : string
      ; bar : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | Foo -> Parser.from_query_required Value_parser.string
      | Bar -> Parser.from_query_required Value_parser.int
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  module Parent = struct
    type t =
      { a : Nested.t
      ; b : Nested.t
      }
    [@@deriving typed_fields, equal, sexp]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.Record.make (module Nested)
      | B -> Parser.Record.make (module Nested)
    ;;

    module Path_order = Parser.Record.Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module Parent)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────────────────────┐
      │ All urls                                                │
      ├─────────────────────────────────────────────────────────┤
      │ /?a.bar=<int>&a.foo=<string>&b.bar=<int>&b.foo=<string> │
      └─────────────────────────────────────────────────────────┘ |}]
  ;;
```

Records nested inside of other records are namespaced on top of each other, but the
namespace can be changed/be overridden with the optional parameter `~namespace` on
`Parser.Record.make`.

## Variants

If you want to parse/unparse your URL or a part of your URL as a variant you can use the
`Parser.Variant` module which needs you to derive `typed_variants` on your variant type.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=variant_1 -->
```ocaml
  module My_google_clone = struct
    type t =
      | Homepage
      | Search
      | Maps
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.unit
      | Search -> Parser.unit
      | Maps -> Parser.unit
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌───────────┐
    │ All urls  │
    ├───────────┤
    │ /homepage │
    │ /maps     │
    │ /search   │
    └───────────┘ |}]
  ;;
```

<aside>
`Parser.unit` is a no-op parser for when you don't need to parse anything. Its type is
`unit Parser.t` and is useful when dealing with variant constructors that don't have a
payload.
</aside>

It would be nice if we could express a homepage as `google.com/` instead of
`google.com/homepage`.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=variant_2 -->
```ocaml
  module My_google_clone = struct
    type t =
      | Homepage
      | Search
      | Maps
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.end_of_path Parser.unit
      | Search -> Parser.unit
      | Maps -> Parser.unit
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌──────────┐
    │ All urls │
    ├──────────┤
    │ /        │
    │ /maps    │
    │ /search  │
    └──────────┘ |}]
  ;;
```

<aside>
The path identifiers for `Maps` and `Search` were inferred with the help of
`typed_variants`, and the homepage identifier was overridden with `end_of_path`. When
something gets inferred vs when it's overridden can be tricky to reason about, but the way
it works is that the "pattern" to match will be inferred unless the following combinators
are used:
- `with_remaining_path pattern` expects the remaining path to be equal to `pattern`.
- `with_prefix pattern` expects the remaining path to have `pattern` as its prefix.
- `end_of_path` expect the remaining path to be empty.
</aside>


It would be nice if we had payload on the URL like `google.com/search?query=capybara` or
`google.com/maps?location=New_York`. This can be accomplished with the same combinators
that the previous `Parser.Record` example used.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=variant_3 -->
```ocaml
  module My_google_clone = struct
    type t =
      | Homepage
      | Search of string
      | Maps of string
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Homepage -> Parser.end_of_path Parser.unit
      | Search -> Parser.from_query_required ~key:"query" Value_parser.string
      | Maps -> Parser.from_query_required ~key:"location" Value_parser.string
    ;;
  end

  let parser = Parser.Variant.make (module My_google_clone)

  let%expect_test _ =
    Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /                       │
    │ /maps?location=<string> │
    │ /search?query=<string>  │
    └─────────────────────────┘ |}]
  ;;
```

## Static checks on your url parsing/unparsing

`Parser.check_ok_and_print_urls_or_errors` will check for any ambiguity/weirdness in your
parser. You'll also be able to see if the urls your site supports changes unexpectedly.
The library tends to infer things off of the names of your types. For instance, paths are
sometimes inferred from constructor names, but can be overridden with `end_of_path`,
`with_prefix` or `with_remaining_path`. The rules for how this works are specified in the
MLI, but it's a lot nicer to explicitly see the shapes of your url in an expect test.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=detected_error -->
```ocaml
module Ambiguous_url = struct
  type t =
    | Foo
    | Bar
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Foo -> Parser.with_prefix [ "x" ] Parser.unit
    | Bar -> Parser.with_prefix [ "x" ] Parser.unit
  ;;
end

let parser = Parser.Variant.make (module Ambiguous_url)

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌─────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                              │ Error message                                                                            │
    ├─────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Ambiguous choices for picking variant constructor check │ ("Duplicate patterns found!"                                                             │
    │                                                         │  (duplicate_patterns (((pattern ((Match x))) (needed_match Prefix)))))                   │
    │ Duplicate urls check                                    │ ("Ambiguous, duplicate urls expressed in parser! This was probably caused due to conflic │
    │                                                         │ ting renames with [with_prefix] or [with_remaining_path]."                               │
    │                                                         │  (duplicate_urls (/x)))                                                                  │
    └─────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
```

## No more broken links in documents

As your site changes, it's likely that your parser will also change, which introduces the
possibility that old urls no longer parse! Broken urls in your users' documents is not
fun, but you can guard against this by using `Versioned_parser` to "stabilize" them.

<!-- $MDX file=../../examples/url_var/lib/url_example.ml,part=versioning -->
```ocaml
module V1 = struct
  type t = Foo of string [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Foo -> Parser.from_path Value_parser.string
  ;;
end

let v1_parser = Versioned_parser.first_parser (Parser.Variant.make (module V1))

module V2 = struct
  type t = New_foo of string [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | New_foo -> Parser.from_path Value_parser.string
  ;;
end

let v2_parser =
  Versioned_parser.new_parser
    (Parser.Variant.make (module V2))
    ~previous:v1_parser
    ~f:(function
      | Foo s -> New_foo s)
;;

let%expect_test _ =
  Versioned_parser.check_ok_and_print_urls_or_errors v2_parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────┐
    │ All urls          │
    ├───────────────────┤
    │ /new_foo/<string> │
    └───────────────────┘

           |
    falls back to
           |
           v

    URL parser looks good!
    ┌───────────────┐
    │ All urls      │
    ├───────────────┤
    │ /foo/<string> │
    └───────────────┘ |}]
;;
```

If you're migrating your site from the non-typed URL var into the `Typed` version
of the API, then you cannot use `Versioned_parser.first_parser` since you won't already
have a `Typed.Parser`. But no worries! You can use `Versioned_parsed.of_non_typed_parser`
instead of `Versioned_parser.first_parser`

## More resources

The MLI is quite long, so here are some pointers to save you time!

- `I want my URL type to be a record!` -> You should look for `Typed.Record.make` in the
  MLI/demo page.
- `I want my URL to take things from the path (e.g. "/profile/capybara_lover123" or
  "/profile/chinchilla_was_taken" or "profile/<string>")` -> You can use
  `Parser.from_path` and `Parser.from_remaining_path` for the "variable parts" of the
  path and `with_prefix` and `with_remaining_path` for the "constant" parts. Examples are
  in demo page + better explanation in MLI.
- `I want my URL type to be made up of something other than the provided primitives!` ->
  `Value_parser.project` or `Parser.project` should do the trick!
- `What are all the provided primitives??` -> These primitives are located and documented
  in `Value_parser`.
- `What order do the parsers/unparsers run?` -> Preorder-like order. Records' fields are
  weirder since the order you parse the record's fields might lead to different results,
  you can specify the order through `path_order`.
- `I want my [Value_parser] to recover from failure.` -> You can use
  `Value_parser.fallback` to provide a default value to use when the parser fails.
