# Views and Forms for Sexpable Types

The `Bonsai_web_ui_auto_generated` library provides functions for creating web UIs and
forms for types that derive `Sexp_grammar`.

# Generating Web UIs

<div style="background-color:#afd7ff; border-style:solid;
padding-left:1em; padding-right:1em; font-size: calc(1em * 1.1);">
The UI generation code is still undergoing development. In particular, the structure of
the generated Vdom may change significantly without notice. You should not take a
dependency on the UI generation if your app relies on the result remaining the same.
</div>

`Bonsai_web_ui_auto_generated.view` takes a `'a Sexp_grammar.t` and a `'a Value.t` and
produces a `Vdom.Node.t` representing the value.

# Generating Forms

`Bonsai_web_ui_auto_generated.form` takes a type that derives `Sexp_grammar` and produces
a form for creating values of that type.
