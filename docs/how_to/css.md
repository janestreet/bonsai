# Css: Styling Your UI

"Styling" determines the layout and presentation of a web application.

```{=html}
<aside>
```
This guide chapter won't attempt to explain CSS and will instead focus
on styling *within Bonsai*.

Here are some free resources for learning CSS:

-   [Mozilla docs](https://developer.mozilla.org/en-US/docs/Learn/CSS)
-   [web.dev docs](https://web.dev/learn/css/)
-   [Flexbox froggy](https://flexboxfroggy.com/)

```{=html}
</aside>
```
## Inline PPX CSS

`ppx_css` allows you to write CSS in your `.ml` files, automatically
loading your styles into the page.

If you had a `<div class="button">..</div>`, it's easy to imagine
accidentially writing CSS rules for `.button` multiple times across the
codebase. To avoid this, `ppx_css` will generate unique selectors that
will not collide.

Using `[%css {|CSS_PROPERTIES|}]` returns a `Vdom.Attr.t`, which will
add a newly created CSS class with the given styles to the `Vdom.Node.t`
it is attached to:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_inline -->
```
``` ocaml
let view =
  Vdom.Node.div
    ~attrs:
      [ [%css
        {|
          background-color: tomato;
          min-width: 2rem;
          min-height: 2rem;
        |}]
      ]
    [ Vdom.Node.text "Very Red Background" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_inline">
```
```{=html}
</iframe>
```
It also supports string interpolation, [similar to
`ppx_string`](https://github.com/janestreet/ppx_string):

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_inline_interpol -->
```
``` ocaml
let box_with_border (color : Css_gen.Color.t) (width : Css_gen.Length.t) =
  Vdom.Node.div
    ~attrs:
      [ [%css
        {|
          border: %{width#Css_gen.Length} solid %{color#Css_gen.Color};
        |}]
      ]
    [ Vdom.Node.text "Nice Borders!" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_inline_interpol">
```
```{=html}
</iframe>
```
While you can interpolate raw `string`s, it is highly recommended to use
the types in the `css_gen` library instead.

You can also use pseudo-selectors, since ppx_css supports [nested
css](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_nesting/Using_CSS_nesting).

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_inline_nesting -->
```
``` ocaml
let hoverable_blocks =
  let block =
    Vdom.Node.div
      ~attrs:
        [ [%css
          {|
            background-color: green;
            min-width: 2rem;
            min-height: 2rem;
            border: 1px solid black;

            &:hover {
              background-color: tomato;
            }

            &:not(:nth-child(odd)):hover {
              background-color: purple;
            }
          |}]
        ]
      [ Vdom.Node.text "Hoverable" ]
  in
  Vdom.Node.div (List.init 6 ~f:(fun _ -> block))
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_inline_nesting">
```
```{=html}
</iframe>
```
You can split up your styling into a bunch of `[%css {||}]` calls:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_inline_multiple -->
```
``` ocaml
let multiple_ppx_css =
  Vdom.Node.div
    ~attrs:[ [%css {|color: red|}] ]
    [ Vdom.Node.text "Foo"
    ; Vdom.Node.div ~attrs:[ [%css "color: blue"] ] [ Vdom.Node.text "Bar" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_inline_multiple">
```
```{=html}
</iframe>
```
This helps avoid a lot of unintentional dependencies on the structure of
your HTML, though [inherited CSS
properties](https://web.dev/learn/css/inheritance) will still cause some
dependencies.

### Downsides

The inline `ppx_css` expression syntax is nice and concise, but but does
not support 1.
[Layers](https://developer.mozilla.org/en-US/docs/Web/CSS/@layer) 2.
[media blocks](https://developer.mozilla.org/en-US/docs/Web/CSS/@media)
3. other
[at-rules](https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule)

## PPX CSS Stylesheets

For more complex rules, `[%css stylesheet {|MULTIPLE_CSS_RULES_HERE|}]`
accepts an entire style sheet and produces a module containing acessors
for the ids, classes, and variables defined within.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_stylesheet -->
```
``` ocaml
module Style =
  [%css
    stylesheet
      {|
@media only screen and (max-width: 300px) {
  .container {
    display: none;
  }
}

@media only screen and (min-width: 300px) {
  .container {
    font-size: 10px;
  }
}

@media only screen and (min-width: 600px) {
  .container {
    font-size: 20px;
  }
}
    |}]

let stylesheet_demo = Vdom.Node.div ~attrs:[ Style.container ] [ Vdom.Node.text "Hello" ]
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_stylesheet">
```
```{=html}
</iframe>
```
It supports interpolation, just like the inline `[%css {||}]`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_stylesheet_interpol -->
```
``` ocaml
let stylesheet_interpol small_bg large_bg =
  let module Style =
    [%css
      stylesheet
        {|
@media only screen and (max-width: 1200px) {
  .container {
    background-color: %{small_bg#Css_gen.Color};
  }
}

@media only screen and (min-width: 1200px) {
  .container {
    background-color: %{large_bg#Css_gen.Color};
  }
}
  |}]
  in
  Vdom.Node.div ~attrs:[ Style.container ] [ Vdom.Node.text "Hello" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_stylesheet_interpol">
```
```{=html}
</iframe>
```
All classnames used in `[%css stylesheet {||}]` blocks will be "hashed"
to avoid naming collisions, so in the example above, `.container` will
actually be something like `.container_hash_099cf63fc3`. There exist
[config options](https://github.com/janestreet/ppx_css) to exclude some
identifiers from hashing.

### CSS Variables

If any `var(...)`s are used in a `[%css stylesheet {||}]` block, a
setter will be made available through a `Style.Variables` module:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=ppx_css_stylesheet_vars -->
```
``` ocaml
  module Style =
    [%css
      stylesheet
        {|
@media only screen and (max-width: 1200px) {
  .container {
    background-color: var(--small-bg);
  }
}

@media only screen and (min-width: 1200px) {
  .container {
    background-color: var(--large-bg);
  }
}
|}]

  let stylesheet_vars =
    Vdom.Node.div
      ~attrs:
        [ Style.container; Style.Variables.set_all ~large_bg:"green" ~small_bg:"purple" ]
      [ Vdom.Node.text "Hello" ]
  ;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#ppx_css_stylesheet_vars">
```
```{=html}
</iframe>
```
Just like classnames, CSS variable names in `[%css stylesheet {||}]`
blocks will be hashed to avoid collisions.

We strongly recommend interpolation over direct variables for several
reasons:

-   It's easy to forget to call `Variables.set_all`, or call it
    somewhere you wouldn't expect to find it.
-   Interpolation encourages use of the `Css_gen.*` types

Interpolation is implemented via CSS variables, so there is no
performance downside.

## Inline Styles

There is an API for setting inline styles on vdom nodes directly with
`Vdom.Attr.style`:

```{=html}
<!-- $MDX skip -->
```
    val Vdom.Attr.style : Css_gen.t -> Vdom.Attr.t

We've already seen the `Css_gen` library, but until now have just been
using it to for its stringification functions to use with `ppx_css`'s
string interpolation syntax. But there are also constructors for making
`Css_gen.t`s, which can be combined via the `@>` operator:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/css_examples.ml,part=css_gen_inline -->
```
``` ocaml
let css_gen_inline =
  let style : Css_gen.t =
    let open Css_gen in
    font_size (`Px 15) @> background_color (`Name "red")
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.style style ] [ Vdom.Node.text "Hello" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#css_gen_inline">
```
```{=html}
</iframe>
```
This is much more verbose and hard to read than `ppx_css`, and is
missing many css attributes. It also doesn't support pseudo-selectors.

# Conclusion

When writing css, we recommend inline `[%css {||}]` attrs, falling back
to `[%css stylesheet {||}]` blocks or `.css` files if you need the
support for at-rules.

Try to avoid use of `Css_gen` inline styles unless absolutely necessary.
