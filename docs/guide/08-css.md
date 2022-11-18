# 08 - Css

Styling an application is very important. Not only is it critical for
making an app *look* good, but "styling" is also responsible for
component layout.

However, there are a few different ways to include style information in
your app or library, each with their own tradeoffs:

1.  Hand-written .css files
2.  `style` properties in vdom nodes
3.  `ppx_css` for inline stylesheets

# Handwritten .css Files

Using .css files to style components and pages is very common in web
development. The Mozilla Developer Network has a very good [introductory
tutorial](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/CSS_basics)
on CSS if you aren't already familiar with it. Google also has a
[comprehensive tutorial](https://web.dev/learn/css/).

At its core, a css stylesheet is a language for pattern-matching on DOM
tree structure combined with a set of rules that should be applied to
matching elements.

With this css: `<!-- $MDX skip -->`{=html}

``` css
table {
  border-collapse: collapse;
}

table td {
  padding: 4px;
}

table thead {
  text-align: center;
  background: brown;
  color: antiquewhite;
  font-weight: bold;
}

table tr {
  background: antiquewhite;
}

table tr:nth-child(even) {
  background: wheat;
}
```

And some basic table code, we get a pretty table!

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=politician_table -->
```
``` ocaml
type row =
  { id : int
  ; name : string
  ; age : int
  }

let basic_table rows =
  let open Vdom.Node in
  let thead = thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ] in
  let tbody =
    rows
    |> List.map ~f:(fun { id; name; age } ->
      tr [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
    |> tbody
  in
  table [ thead; tbody ]
;;

let politicians =
  basic_table
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#politician-table">
```
```{=html}
</iframe>
```
By keeping the styling logic and vdom-node production separate, our css
can be quite high-level. Sadly, the "high level description" of the
pattern for this table might be more broad than we had hoped; this css
will style *every* table in our application, even those created by other
components that you might not own!

A common fix for this problem is to make the patterns more specific by
adding a unique class name targeting just the tables that we want. For
example:

```{=html}
<!-- $MDX skip -->
```
``` diff
- table {
+ table.politicians {
    border-collapse: collapse;
  }

- table td {
+ table.politicians td {
    padding: 4px;
  }

- table thead {
+ table.politicians thead {
    text-align: center;
    background: brown;
    color: antiquewhite;
    font-weight: bold;
  }

- table tr {
+ table.politicians tr {
    background: antiquewhite;
  }

- table tr:nth-child(even) {
+ table.politicians tr:nth-child(even) {
    background: wheat;
  }
```

and then in the table production code,

```{=html}
<!-- $MDX skip -->
```
``` diff
- table [ thead; tbody ]
+ table ~attr:(Vdom.Attr.class_ "politician") [ thead; tbody ]
```

This solves the issue of our styles inadvertantly obliterating other
people's components, but comes at the cost of verbosity. It also isn't
completely foolproof: if two people pick the same identifier (in this
case we went with `politician`), then the clash would still occur. This
leads to people using very long and descriptive identifiers to reduce
the odds of a collision.

Another downside is that if you're an application author pulling in a UI
component which has a stylesheet, then you need to somehow get that css
file into your application. Typically this is done by writing a dune
rule that concatenates your application's stylesheet with the
stylesheets of any dependencies, like so:

```{=html}
<!-- $MDX skip -->
```
``` lisp
(rule (
  (targets (style.css))
  (deps    (%{root}/lib/dygraph/dist/dygraph.css ./my_styles.css))
  (action "cat %{deps} > %{target}")))
```

If a component requires a stylesheet, there is no way of knowing that
fact (other than by reading the readmes, but who does that?).

**Pros**

1.  Full access to CSS language including pseudoselectors (like
    `:nth-child(even)`, or `:hover`)
2.  Good debugging support in Chrome Devtools

**Cons**

1.  Identifier collisions can break things in subtle ways
2.  Reusable components that rely on a css stylesheet force the
    application author to use css files and to build out the dune rule
    for concatenating all of their dependencies' css.

# Vdom.Attr.style

Another way to add styling to DOM nodes is through an individual DOM
node's `style` property. `Vdom.Attr.style` has this type signature:
`Css_gen.t -> Vdom.Attr.t`, so we'll be primarily looking at the
`Css_gen`.

A `Css_gen.t` is a collection of key-value pairs of css properties and
their values. As an example,

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let style: Css_gen.t = Css_gen.text_align `Center
```

is a style that only contains the kv-pair `text-align: center;`.
Meanwhile,

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let style: Css_gen.t =
  let open Css_gen in
  text_align `Center @> background_color (`Name "red")
```

makes use of the `@>` operator in order to merge two `Css_gen.t`'s,
producing kv-pairs which contain `text-align: center; background: red;`.

If we ported the table example to use the inline style attribute, our
code would now look like this:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=attr_table -->
```
``` ocaml
type row2 =
  { id : int
  ; name : string
  ; age : int
  }

let table_styles =
  let open Css_gen in
  border_collapse `Collapse
  @> border ~style:`Solid ~color:(`Name "brown") ~width:(`Px 1) ()
;;

let thead_styles =
  let open Css_gen in
  text_align `Center
  @> background_color (`Name "brown")
  @> color (`Name "antiquewhite")
  @> font_weight `Bold
;;

let tr_odd = Css_gen.background_color (`Name "antiquewhite")
let tr_even = Css_gen.background_color (`Name "wheat")

let td_styles =
  Css_gen.padding ~top:(`Px 4) ~bottom:(`Px 4) ~left:(`Px 4) ~right:(`Px 4) ()
;;

let basic_table_attr rows =
  let open Vdom.Node in
  let thead =
    thead
      ~attr:(Vdom.Attr.style thead_styles)
      [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ]
  in
  let tbody =
    rows
    |> List.mapi ~f:(fun i { id; name; age } ->
      let tr_style = if Int.( % ) i 2 = 0 then tr_even else tr_odd in
      tr
        ~attr:(Vdom.Attr.style tr_style)
        [ td ~attr:(Vdom.Attr.style td_styles) [ textf "%d" id ]
        ; td ~attr:(Vdom.Attr.style td_styles) [ text name ]
        ; td ~attr:(Vdom.Attr.style td_styles) [ textf "%d" age ]
        ])
    |> tbody
  in
  table ~attr:(Vdom.Attr.style table_styles) [ thead; tbody ]
;;

let politicians =
  basic_table_attr
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;
```

Because we're no longer using a css file, the first part of the file
re-defines all of the styling in OCaml using `Css_gen`. However, these
styles are not applied automatically, so we also need to add calls to
`Vdom.Attr.style` everywhere.

In a particularly egregious case, we changed from using `List.map` to
`List.mapi`, which allowed us to check if the row is even or odd,
something that was previously done for us by our stylesheet. This brings
us to our first major drawback of using inline style attributes:
pseudo-selectors are unavailable. Some of these, like `:nth-child`, are
implementable in our view-calculation logic because we're manually
constructing the lists anyway. However, other pseudo-selectors like
`:hover` or `:focus` are impossible, and others, like `:nth-of-type` are
possible, but hair-pullingly annoying.

**Pros**

1.  No need to worry about identifier clashes because styles are stuck
    directly on the nodes themselves.
2.  If used in a library, the styles come with the library instead of
    being another css file that library users need to know about and
    manage.

**Cons**

1.  Much more verbose.
2.  Many css-attributes are missing from the `Css_gen` library (you can
    work around this with `Css_gen.create`).
3.  Pseudo-selectors just aren't available.

# CSS Ppx

The third option is to use a brand new ppx: `ppx_css`! With this ppx,
you can write css code in your .ml files, and it will be loaded into the
document at page-load. To use the ppx, add it to your jbuild like so:

``` diff
 (executables (
   (names (main))
   (libraries (bonsai_web))
+  (preprocess (pps (ppx_jane ppx_css)))
   (js_of_ocaml ())))
```

And now you can bind a module to the result of a css ppx invocation:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=inline_css -->
```
``` ocaml
module Style =
  [%css
    stylesheet
      {|
table.politicians {
  border-collapse: collapse;
  border: 1px solid brown;
}

table.politicians td {
  padding: 4px;
}

table.politicians thead {
  text-align: center;
  background: brown;
  color: antiquewhite;
  font-weight: bold;
}

table.politicians tr {
  background: antiquewhite;
}

table.politicians tr:nth-child(even) {
  background: wheat;
}
    |}]
```

This is the exact same css from the second example! Notice that it still
has a "politicians" class before every rule. With this new `Style`
module bound, we can *almost* keep the same ocaml view generation as we
had originally:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=table_with_ppx_css -->
```
``` ocaml
let table_with_ppx_css rows =
  let open Vdom.Node in
  let thead = thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ] in
  let tbody =
    rows
    |> List.map ~f:(fun { id; name; age } ->
      tr [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
    |> tbody
  in
  table ~attr:Style.politicians [ thead; tbody ]
;;
```

The only difference between this function and the first one we wrote is
this:

```{=html}
<!-- $MDX skip -->
```
``` diff
- table ~attr:(Vdom.Attr.class_ "politicians") [ thead; tbody ]
+ table ~attr:(Vdom.Attr.class_ Style.politicians) [ thead; tbody ]
```

This `Style.politicians` value was generated by the ppx because it
noticed that we were using it as a class-name. The ppx will also
uniquify all of the class names and ids that it finds in the stylesheet.
As an example, the first rule's selector will become
`table.politicians_hash_e82ee99238`, where the unique name is generated
by hashing the contents of the css string as well as the path to the
file containing the ppx. This means that `Style.politicians` is a string
with the value `politicians_hash_e82ee99238`, which we use for the
classname.

This kind of identifier hashing is useful because it means that
component authors don't need to worry about collisions.

# \[\~rewrite\] flag

Sometimes, like when interacting with customization APIs that require
specific classnames for CSS customization, ppx_css's hygenic identifier
hashing could get in your way. With \[\~rewrite\], you get to choose the
name for an identifier rather than ppx_css choosing - or rather
hashing - it for you.

```{=html}
<!-- $MDX skip -->
```
``` ocaml
(* A table library's customization API _needs_ users to style the "table-header" class. *)
stylesheet {|.table-header {...}|}
```

You can disable hashing when needed by using the optional `~rewrite`
parameter.

```{=html}
<!-- $MDX skip -->
```
``` ocaml
(* Scenario: A table library's customization API _needs_ users to style the "table-header" class. *)
stylesheet ~rewrite:["table-header", "table-header"] {|.table-header {...}|}
```

The above segment will "rewrite" `table-header` into `table-header`
overriding the default hashing behavior.

Some other times, you might still want hygenic hashing, but need the
same identifier to have the same hash between two specific \[%css
stylesheet\] invocations. Here are some examples of the rewrite flag in
action:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
stylesheet ~rewrite:[ "table-header", "table-header"; "table_row", "table-row" ] {|...|} (* Rewrites multiple identifiers at once. *)
stylesheet ~rewrite:[ "my_table", My_table_component.table ] {|...|} (* References an identifier defined in another module *) |xxx}];
```

# Theming through PPX CSS

One additional benefit is that this ppx/inliner opens up doors for
allowing components to be customized by their users. The `Style` module
that the ppx derived actually has this signature:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
sig
  module type S  = sig
    val politicians : string
  end

  type t = (module S)

  val default : t
  val politicians : string
end
```

While we were just using the default `politicians` value, the module
type and a default packed module implementing that module type are
intended to be used for making it easy for component authors to allow
their users to theme the components.

By tweaking the code slightly we can see how this is possible:

```{=html}
<!-- $MDX skip -->
```
``` diff
- let table_with_ppx_css rows =
+ let table_with_ppx_css ?(theme=Style.default) rows =
+   let module Style = (val theme) in
    let open Vdom.Node in
    let thead =
      thead [ td [ text "id" ]; td [ text "name" ]; td [ text "age" ] ]
    in
    let tbody =
      rows
      |> List.map ~f:(fun { id; name; age } ->
        tr
          [ td [ textf "%d" id ]; td [ text name ]; td [ textf "%d" age ] ])
      |> tbody
    in
    table ~attr:(Vdom.Attr.class_ Style.politicians) [ thead; tbody ]
  ;;
```

now someone could make a new module which implements that type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=my_theme -->
```
``` ocaml
module My_theme =
  [%css
    stylesheet
      {|
table.politicians {
  border-collapse: collapse;
  border: 1px solid black;
}

table.politicians td {
  padding: 4px;
}

table.politicians thead {
  text-align: center;
  background: black;
  color: white;
  font-weight: bold;
}

table.politicians td {
  border: 1px solid black;
}

    |}]

let table =
  themeable_table
    ~theme:(module My_theme)
    [ { id = 0; name = "George Washington"; age = 67 }
    ; { id = 1; name = "Alexander Hamilton"; age = 47 }
    ; { id = 2; name = "Abraham Lincoln"; age = 56 }
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#themeable-table">
```
```{=html}
</iframe>
```
Due to the nature of the generated code, there are a few restrictions on
the person building a custom theme:

1.  No new identifiers are allowed. This means that if a component
    author uses a dumb name like "politicians", then a theme author
    needs to as well.
2.  *All* identifiers present in the component definition must be used.
    You can use an empty rule like `.politicians {}` in order to meet
    this requirement if you wanted to avoid themeing something.

These restrictions may seem onerous, but it's a lot like using a record
in OCaml: the author of the record gets to decide what the names of the
fields are, and if someone wants to construct a value of that record,
they can't just leave out the fields that they don't care about. This
means that component authors should think ahead and ask themselves "what
kind of customizability will users of this component want?" before
settling on a set of identifiers and picking which nodes they're
attached to.

There are (plans for) other kinds of ppx transformations other than just
the identifier re-writing, such as automatically introducing the
namespacing classnames for you. Those aren't here yet, but they might be
soon.

It should be noted that even with the classname hashing, it's still
possible to write rules that affect more than you might like. I actually
included a number of bugs of this form in the examples so far (!).
`.table.politicians td` affects *every* td inside of the table, even
tables within tables. So if one of the cells of this table contained
another table that was styled differently, we'd run the risk of
overwriting it. The "correct" rule in this case would be to fully
qualify the paths to the elements, so `table.politicians td` would
become `table.politicians > tbody > tr > td`.

**Pros**

1.  Full access to CSS language including pseudoselectors (like
    `:nth-child(even)`, or `:hover`)
2.  Good debugging support in Chrome Devtools
3.  No need to worry about identifier clashes because these identifiers
    are hashed before being used.
4.  If used in a library, the styles come with the library instead of
    being another css file that library users need to know about and
    manage.

**Cons**

1.  It's still up to you to make sure that styles aren't too general
    (this is going to be addresed in further releases of the ppx.
