# State Per Key

`Bonsai.scope_model` allows us to store multiple copies of state for
some `Bonsai.t`, each keyed by some value, while only displaying one at
a time.

## Multiple copies with assoc

Imagine we'd like to have a counter for each of many users. We could
show all the counters with an `assoc`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/scope_model_examples.ml,part=counters_for_users_assoc -->
```
``` ocaml
let counters_for_users_assoc (local_ graph) : Vdom.Node.t Bonsai.t =
  let users =
    [ "Alice", (); "Bob", (); "Charlie", () ] |> String.Map.of_alist_exn |> Bonsai.return
  in
  let counters =
    Bonsai.assoc
      (module String)
      users
      ~f:(fun _ _ graph -> State_examples.counter_ui graph)
      graph
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

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counters_for_users_assoc">
```
```{=html}
</iframe>
```
## One at a time

But what if we wanted to show one counter at a time, but still maintain
separate counter state for each user? We can use `Bonsai.scope_model`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/scope_model_examples.ml,part=counters_for_users_scoped -->
```
``` ocaml
module Form = Bonsai_web_ui_form.With_automatic_view

let counters_for_users_scoped (local_ graph) : Vdom.Node.t Bonsai.t =
  let form =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "Alice"; "Bob"; "Charlie" ])
      graph
  in
  let active_user =
    let%arr form = form in
    Form.value_or_default form ~default:"Alice"
  in
  Bonsai.scope_model
    (module String)
    ~on:active_user
    graph
    ~for_:(fun graph ->
      let%arr counter = State_examples.counter_ui graph
      and name = active_user
      and form = form in
      Vdom.Node.div
        [ Form.view_as_vdom form; Vdom.Node.p [ Vdom.Node.text name ]; counter ])
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#counters_for_users_scoped">
```
```{=html}
</iframe>
```
Bonsai will maintain a separate copy of all state created in `for_` for
every key that is passed into `on`.

Of course, in this case, we could have centralized state for all
counters. But this would require explicitly threading through
getters/setters, and wouldn't allow us to use any code that creates and
owns its own state.

## `Scope_model` and Lifecycles

`scope_model` will run any `on_deactivate` and `on_activate` [lifecycle
handlers](./lifecycles.mdx) defined inside of `for_` whenever `on`
changes. `on_deactivate` will run first, and have access to state
corresponding to the old value of `on`. Then, `on_activate` will run
with access to state for the new `on`.

```{=html}
<aside>
```
We also used [Bonsai's form library](./forms.mdx)!
```{=html}
</aside>
```
## The Underlying Machinery

Recall from the [state chapter](../guide/04-state.mdx) that Bonsai
aggregates and stores all state centrally. But `Bonsai.assoc`s need a
dynamic number state copies: one for each key. These states all have the
same static structure though, so we can just use a big `Map.t`.

`scope_model` is implemented as a `Bonsai.assoc`, who's input map is
`Map.of_alist_exn [THE_CURRENT_KEY, ()]`: that's how we get state
retention and lifecycles on key change.
