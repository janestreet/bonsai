```{=html}
```
# Forms

Bonsai has an entire library dedicated to building and combining forms
called `bonsai_web_ui_form`.

For the rest of this doc, this module alias will be in effect:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
module Form = Bonsai_web_ui_form.With_automatic_view
```

# Form.t

The primary type in the forms library is `'a Form.t`. A value of type
`'a Form.t` represents the state of a form at one particular instant in
time, where the form in question can edit values of type `'a`.

Because of the inherently stateful nature of form UIs, form building
functions take `(local_ graph)`. For example, a textbox form element
that produces strings has type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Form.Elements.Textbox.string : unit -> local_ Bonsai.graph -> string Form.t Bonsai.t
```

And the type for a checkbox that produces bools has this type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Form.Elements.Checkbox.bool
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> default:bool
  -> unit
  -> local_ Bonsai.graph
  -> bool Form.t Bonsai.t
```

There are three primary operations that can be performed on a
`'a Form.t`

1.  Extract the current value:
    `val Form.value: 'a Form.t -> 'a Or_error.t`
2.  Compute the view: `val Form.view_as_vdom: 'a Form.t -> Vdom.Node.t`
3.  Set the value of the form:
    `val Form.set: 'a Form.t -> 'a -> unit Vdom.Effect.t`

```{=html}
<aside>
```
The `view_as_vdom` function actually has an optional parameter, which
we'll investigate soon.
```{=html}
</aside>
```
Let's look at them all in action, using the textbox from above as an
example:

## Form.value

The "value" of a `'a Form.t` is the current output of the form as filled
in by the user. For a simple textbox, that value would be `string`, but
most forms are compositions of subforms, so the produced value can be a
record or variant.

In the following example, the value of a textbox is extracted and
printed as a sexp:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=form_textbox_value -->
```
``` ocaml
let textbox_value (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox >>| Form.label "my textbox" in
  let value = Form.value textbox in
  Vdom.Node.div
    [ Form.view_as_vdom textbox
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_textbox_value">
```
```{=html}
</iframe>
```
Forms returning their values inside of an `Or_error.t` may be surprising
at first, but in practice, more complex forms are fallible, either
because form validation for an element has failed, or because a large
form is only partially filled out. By making the assumption that *all*
forms are fallible, the rest of the API is simpler.

## Form.view_as_vdom

`view_as_vdom` renders the form into a `Vdom.Node.t`. However, it also
has an optional parameter that makes submitting the form easier. Its
full type signature is:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
module Submit : sig
  type 'a t

  val create
    :  ?handle_enter:bool
    -> ?button:string option
    -> f:('a -> unit Ui_effect.t)
    -> unit
    -> 'a t
end

val view_as_vdom : ?on_submit:'a Submit.t -> 'a t -> Vdom.Node.t
```

Because forms are frequently paired with a "submit" button, the optional
submission options provide an easy way to submit the form, with the `f`
field being called with the value of the fully-validated form. The two
options for submitting the form are

1.  `handle_enter`, when `true` will cause the form to be submitted
    whenever the `<enter>` key is pressed while focusing on a form
    element inside this form.
2.  `button`, if `Some`, a button with the provided text will be added
    to the form. This button will be disabled whenever the form is in an
    error state, but when the form is valid, the button will be enabled
    and will trigger the submission function when pressed.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=view_with_submission -->
```
``` ocaml
let textbox_on_submit (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox in
  textbox
  |> Form.label "text to alert"
  |> Form.view_as_vdom ~on_submit:(Form.Submit.create () ~f:alert)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_textbox_on_submit">
```
```{=html}
</iframe>
```
## Form.set

Setting the contents of a form is a rarer requirement. Most forms are
read-only (the user is the only one filling it out), but sometimes, a
form should be modified by the program, perhaps to initialize the form
in a specific state.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=form_set -->
```
``` ocaml
let form_set (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox = textbox >>| Form.label "my textbox" in
  Vdom.Node.div
    [ Form.view_as_vdom textbox
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world") ]
        [ Vdom.Node.text "click me" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_set">
```
```{=html}
</iframe>
```
# Combinators

Most forms contain many input elements, and Bonsai-Forms comes with a
set of combinators for combining many smaller subforms into a larger
form.

For this example, we'll build a form for the following type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_form_type -->
```
``` ocaml
type t =
  { some_string : string
  ; an_int : int
  ; on_or_off : bool
  }
[@@deriving typed_fields, sexp_of]
```

Building a form that produces values of this type requires the use of
[`ppx_typed_fields`](https://github.com/janestreet/ppx_typed_fields),
which you'll need to add to your jbuild. Deriving `typed_fields` will
make a module named `Typed_field` containing a type with a constructor
representing each field in the record it was derived on.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_form -->
```
``` ocaml
module Record = struct
  type t =
    { some_string : string
    ; an_int : int
    ; on_or_off : bool
    }
  [@@deriving typed_fields, sexp_of]

  let form : local_ Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Record.make
      (module struct
        (* reimport the module that typed_fields just derived *)
        module Typed_field = Typed_field

        let label_for_field = `Inferred

        (* provide a form computation for each field in the record *)
        let form_for_field
          : type a. a Typed_field.t -> local_ Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field (local_ graph) ->
          match typed_field with
          | Some_string ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
          | An_int ->
            Form.Elements.Number.int
              ~default:0
              ~step:1
              ~allow_updates_when_focused:`Always
              ()
              graph
          | On_or_off -> Form.Elements.Checkbox.bool ~default:false () graph
        ;;
      end)
  ;;
end
```

We can also do the same for variants with `[@@deriving typed_variants]`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=variant_form -->
```
``` ocaml
module Variant = struct
  type t =
    | A
    | B of int
    | C of string
  [@@deriving typed_variants, sexp_of]

  let form : local_ Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Variant.make
      (module struct
        (* reimport the module that typed_fields just derived *)
        module Typed_variant = Typed_variant

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor

        (* provide a form computation for constructor in the variant *)
        let form_for_variant
          : type a. a Typed_variant.t -> local_ Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field (local_ graph) ->
          match typed_field with
          | A -> Form.return () |> Bonsai.return
          | B -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
        ;;
      end)
  ;;
end
```

Finally, using this new form and printing the results:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_form_view -->
```
``` ocaml
let view_for_form : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun graph ->
  let%arr record_form = Record.form graph
  and variant_form = Variant.form graph in
  let form = Form.both record_form variant_form in
  let value = Form.value form in
  Vdom.Node.div
    [ Form.view_as_vdom form
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: (Record.t * Variant.t) Or_error.t] value)
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#record_form_view">
```
```{=html}
</iframe>
```
# Projection

Notably missing in the Forms API is a "map" function. In its place is
`Form.project`, which has this type signature:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val project
  :  'a t
  -> parse_exn:('a -> 'b)
  -> unparse:('b -> 'a)
  -> 'b t
```

`project` is a way to move from a form producing values of type `'a` to
a form producing values of type `'b`, but it requires two "mapping"
functions, `parse_exn`, which moves from `'a` to `'b` as you'd expect,
but the other, `unparse`, goes in the opposite direction!

`unparse` is required because `Form.set` needs to be able to accept
values of type `'b` and route them through the setter for the input
form.

In practice, `project` is used to build forms for types that can be
parsed from other types. For example, `Form.Elements.Textbox.int` is
implemented like so:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=int_textbox -->
```
``` ocaml
let int : local_ Bonsai.graph -> int Form.t Bonsai.t =
  fun graph ->
  let form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph in
  let%arr form = form in
  Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#int_textbox">
```
```{=html}
</iframe>
```
You'll notice that non-integers are reported as an error. `Form.project`
captures the exception thrown by `Int.of_string` and the `Form.value`
returned by the `project`ed form is an `Error`.

## Validation is just projection!

You may not realize, but `Form.project` can also be used for validation!
If we take `'b = 'a` in the type signature for `Form.project` above and
provide the identity function for `unparse`, the `parse_exn` function
can signal an invalid or valid state by raising or not raising,
respectively.

Because this is a common pattern, Bonsai Forms provides the following
function, built on top of `Form.project` to help reduce boilerplate:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val validate : 'a t -> f:('a -> unit Or_error.t) -> 'a t
```
