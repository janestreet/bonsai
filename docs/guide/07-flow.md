# 07 - Control Flow

This chapter of the guide is a collection of smaller topics that are
valuable for structuring components.

# Components as DAGs

One of the biggest differences between Bonsai and other virtual-dom
based UI frameworks (such as React, Vue, or Elm) is that Bonsai
structures the composition of UI components as a Directed Acyclic Graph
instead of as a tree.

What this means in practice is that the output of one component can be
fed as input to another component.

To illustrate this, we'll build a textbox component whose placeholder
text is specified dynamically. This textbox component is so similar to
the one constructed in the [state chapter](./03-state.md) that the diff
between that version and the new one is shown below for convenience.

```{=html}
<!-- $MDX skip -->
```
``` diff
-let textbox =
+let textbox ~placeholder =
   let%sub state, set_state = Bonsai.state (module String) ~default_model:"" in
   (let%arr state = state
      and set_state = set_state
+     and placeholder = placeholder in
      let view =
        Vdom.Node.input
          ~attr:(Vdom.Attr.many [ Vdom.Attr.value_prop state
          ; Vdom.Attr.on_input (fun _ new_text -> set_state new_text)
+         ; Vdom.Attr.placeholder placeholder
          ])
          ()
      in
      state, view)
;;
```

And a basic usage of the new component (with a constant placeholder)

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=textbox_with_placeholder -->
```
``` ocaml
let textbox_with_placeholder = textbox ~placeholder:(Value.return "the placeholder")
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_with_placeholder">
```
```{=html}
</iframe>
```
And because of the graph-like structure of a Bonsai app, we can
trivially chain two textboxes together so that the contents of one of
the output of one textbox is used as the placeholder for the next.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=textbox_chaining -->
```
``` ocaml
let textbox_chaining =
  let%sub a_contents, a_view = textbox ~placeholder:(Value.return "") in
  let%sub _, b_view = textbox ~placeholder:a_contents in
  let%arr a_view = a_view
  and b_view = b_view in
  let style = Vdom.Attr.style (Css_gen.display `Inline_grid) in
  Vdom.Node.div ~attr:style [ a_view; b_view ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_chaining">
```
```{=html}
</iframe>
```
Clearly, chaining together two textboxes to set the placeholder text
isn't particularly useful (the examples are small though!), but in real
applications, this kind of component dependency structuring is valuable
in a myriad of ways:

-   The output of a "tab-selector" component could include the view for
    a tab-bar, but also a value for the currently selected tab. Then
    other components could read that value and respond accordingly.
-   A form could dynamically change its contents based on the values of
    previously filled out form contents.
-   At the top of an application component graph, a "light mode or dark
    mode" checkbox component could be added, and the current value
    (either light or dark) could be passed down to downstream components
    to influence the way that they display.

# match%sub

`let%sub` should be familiar to you by now, but there's actually a more
powerful form of variable substitution which permits a limited form of
dynamism, match expressions! With `match%sub`, a `'a Value.t` is matched
on, and any bindings in the match arm are projected out into their
`Value.t` form. Let's look at what that means in practice!

In the following example, we'll avoid building the 2nd textbox if the
first textbox is either empty or only contains whitespace.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=textbox_chaining_match -->
```
``` ocaml
let textbox_matching =
  let%sub a_contents, a_view = textbox ~placeholder:(Value.return "") in
  let a_contents =
    let%map s = a_contents in
    let s = String.strip s in
    if String.is_empty s then None else Some s
  in
  match%sub a_contents with
  | None ->
    let%arr a_view = a_view in
    let message = Vdom.Node.div [ Vdom.Node.text "<a is empty>" ] in
    Vdom.Node.div [ a_view; message ]
  | Some placeholder ->
    let%sub _, b_view = textbox ~placeholder in
    let%arr a_view = a_view
    and b_view = b_view in
    let style = Vdom.Attr.style (Css_gen.display `Inline_grid) in
    Vdom.Node.div ~attr:style [ a_view; b_view ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_chaining_match">
```
```{=html}
</iframe>
```
There are a few details to note about some of the types up above

1.  The matched value has type `'a Value.t`
2.  The values produced by each of the match-arms must be of type
    `'b Computation.t`
3.  The overall type of the `match%sub` expression has type
    `'b Computation.t`
4.  Any identifiers bound during matching (in the above example, this is
    just `placeholder`) are available in `Value.t` form.

It is important to know that at any point in time, only one of the arms
in the pattern match is active, so the components in the not-matched
arms are not being computed.

In addition to `match%sub`, `if%sub` also exists, with the exact same
semantics, but specialized for booleans.

# Bonsai.assoc

Up until now, Bonsai hasn't had any real tools for dealing with
dynamically sized collections of components. Sure, you could manually
re-use a text-box component twice, but if the number of distinct
components is determined at runtime, writing out a bunch of `let%sub`
won't cut it.

That's where `Bonsai.assoc` comes in. Let's start by looking at its type
signature:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val assoc
  :  ('key, 'cmp) comparator
  -> ('key, 'data, 'cmp) Map.t Value.t
  -> f:('key Value.t -> 'data Value.t -> 'result Computation.t)
  -> ('key, 'result, 'cmp) Map.t Computation.t
```

Breaking the parts of the signature down one-by-one we have

1.  `('key, 'cmp) comparator`: A comparator is required; this is
    typically just `(module Int)` or `(module My_type)` where the type
    is comparable, and has the sexp functions defined.
2.  `('key, 'data, 'cmp) Map.t Value.t`: A dynamic map from `'key` to
    `'data`.
3.  a named function `f` with type
    `'key Value.t -> 'data Value.t -> 'result    Computation.t`; this
    function will be called with every key-value pair in the map, and
    produces a computation containing `'result`.
4.  Finally, the return value of `assoc` is
    `('key, 'result, 'cmp) Map.t    Computation.t`, a map from the same
    key as the input to the `'result` produced in the `f` function.

This type signature is remarkably close to the regular OCaml function
`Map.mapi`, which has this type signature:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val mapi
:  ('key, 'data, 'cmp) Map.t
-> f:(key:'key -> data:'data -> 'result)
-> ('key, 'result, 'cmp) Map.t
```

But of course the Bonsai version has a bunch of `Value.t` and
`Computation.t` in it's type signature, so what are those types giving
us?

The first benefit to `assoc` is that the computation inside of `f` is
only evaluated once per key/value pair, after which any updates to the
`data` travel through the regular bonsai `Value` graph optimization.
This means that if the input map is 100,000 elements large, but only one
of the keys has data that is changing frequently, only the one component
for that key will be involved in recomputing the eventual result of the
overall function.

The other benefit to using assoc is apparent from looking at the type of
the function: the `f` function returns a `Computation.t`, which means
that every key/value pair in the input map is its own component, each
with it's own independent state!

For this example, we'll re-use the "counter" component defined in the
last section of the [state chapter](./03-state.md), but this time,
there'll be a bunch of them!

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=multiple_counters -->
```
``` ocaml
let multiple_counters (input : unit String.Map.t Value.t) =
  let%sub counters =
    Bonsai.assoc
      (module String)
      input
      ~f:(fun _key (_ : unit Value.t) -> State_examples.counter_state_machine)
  in
  let%arr counters = counters in
  Vdom.Node.table
    (counters
     |> Map.to_alist
     |> List.map ~f:(fun (key, vdom) ->
       let open Vdom.Node in
       let name = td [ Vdom.Node.text key ] in
       let counter = td [ vdom ] in
       Vdom.Node.tr [ name; counter ]))
;;
```

and to start out with we'll use a constant map as an input to the
component:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=multiple_counters_constant_map -->
```
``` ocaml
let multiple_counters_constant =
  multiple_counters
    ([ "hello", (); "there", () ] |> Map.of_alist_exn (module String) |> Value.return)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#multiple_counters_constant">
```
```{=html}
</iframe>
```
and while this does show off how to associate a component across a map,
using `Value.return` makes it hard to see the "dynamic" aspect. So let's
build a dynamically editable map!

```{=html}
<aside>
```
This final code example is less about `Bonsai.assoc` and more about
integrating concepts from the rest of the bonsai guide:

-   `Bonsai.state_machine` tracks and edits a map for the counters
-   Bonsai's Forms library is used to interact with the state-machine to
    add new entries
-   `assoc` builds up the table of counters from the map inside
    state-machine.

```{=html}
</aside>
```
```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/flow_examples.ml,part=kudo_tracker -->
```
``` ocaml
module Model = struct
  type t = unit String.Map.t [@@deriving sexp, equal]

  let default = String.Map.of_alist_exn [ "Dave", (); "Jill", () ]
end

module Action = struct
  type t =
    | Add of string
    | Remove of string
  [@@deriving sexp_of]
end

let people =
  Bonsai.state_machine0
    (module Model)
    (module Action)
    ~default_model:Model.default
    ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
      match action with
      | Add name -> Map.set model ~key:name ~data:()
      | Remove name -> Map.remove model name)
;;

let add_new_person_form ~inject_add_person =
  let%sub form = Form.Elements.Textbox.string () in
  let%arr form = form
  and inject_add_person = inject_add_person in
  let on_submit name = Vdom.Effect.Many [ Form.set form ""; inject_add_person name ] in
  form
  |> Form.label "name"
  |> Form.validate ~f:(fun name ->
    if String.for_all name ~f:Char.is_whitespace
    then Error (Error.of_string "name must not be empty")
    else Ok ())
  |> Form.view_as_vdom ~on_submit:(Form.Submit.create ~f:on_submit ())
;;

let people_table people ~inject_remove_person =
  Bonsai.assoc
    (module String)
    people
    ~f:(fun name (_ : unit Value.t) ->
      let%sub counter = State_examples.counter_state_machine in
      let%arr counter = counter
      and name = name
      and inject_remove_person = inject_remove_person in
      let open Vdom.Node in
      let remove_person =
        td
          [ button
              ~attr:(Vdom.Attr.on_click (fun _ -> inject_remove_person name))
              [ text "x" ]
          ]
      in
      let name = td [ text name ] in
      let counter = td [ counter ] in
      tr [ name; counter; remove_person ])
;;

let kudo_tracker =
  let%sub people, inject_action = people in
  let%sub form =
    let inject_add_person =
      let%map inject_action = inject_action in
      fun name -> inject_action (Add name)
    in
    add_new_person_form ~inject_add_person
  in
  let%sub people_table =
    let inject_remove_person =
      let%map inject_action = inject_action in
      fun name -> inject_action (Remove name)
    in
    people_table people ~inject_remove_person
  in
  let%arr people_table = people_table
  and form = form in
  let open Vdom.Node in
  div
    [ h2 [ text "kudos tracker" ]
    ; table
        [ thead [ tr [ th [ text "Name" ]; th [ text "# Kudos" ]; th [ text "Remove" ] ] ]
        ; tbody (Map.data people_table)
        ]
    ; h2 [ text "Add Person" ]
    ; form
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#kudo_tracker">
```
```{=html}
</iframe>
```
