# Lifecycles

The `match%sub` and `Bonsai.assoc` [control flow
operators](../guide/05-control_flow.mdx) are very similar to regular
OCaml `match` statements and `Map.map` calls. But because Bonsai is
incremental, their code is constantly being rerun whenever some
dependency changes.

## Active / Inactive Code

When a `match%sub` switches from one branch (A) to another (B), Bonsai
keeps around any state instantiated inside of A. But the output of (A)
isn't used, so its recomputation is paused: (A) is inactive,
until/unless the `match%sub` switches back.

`assoc`s behave similarly: Bonsai maintains a separate copy of state for
every key in the input map. But if some key (K) is removed from the map,
its state is retained, and the code recomputing its output becomes
unactive until/unless the (K) is added back.

```{=html}
<aside>
```
It is possible to [explicitly reset state](./resetting_state.mdx) on
deactivation.
```{=html}
</aside>
```
## Lifecycle Events

`Bonsai.Edge.lifecycle` allows you to schedule `Effect.t`s when the
containing code switches between active / inactive as `on_activate` and
`on_deactivate` arguments:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lifecycle_examples.ml,part=lifecycle -->
```
``` ocaml
let lifecycle_demo (local_ graph) =
  let log_val, log =
    Bonsai.state_machine0
      ~default_model:""
      ~apply_action:(fun _ curr new_ -> curr ^ new_)
      graph
  in
  let show, toggle_show = Bonsai.toggle ~default_model:false graph in
  let main_view =
    match%sub show with
    | true ->
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr log = log in
           log "🚀")
        ~on_deactivate:
          (let%arr log = log in
           log "🔥")
        graph;
      Vdom.Node.text [%string "Active!!!!"] |> Bonsai.return
    | false -> Vdom.Node.text "Nothing to see here..." |> Bonsai.return
  in
  let%arr log_val = log_val
  and toggle_show = toggle_show
  and main_view = main_view in
  Vdom.Node.(
    div
      [ div
          [ button
              ~attrs:[ Vdom.Attr.on_click (fun _ -> Effect.all_unit [ toggle_show ]) ]
              [ text "toggle show" ]
          ; text log_val
          ]
      ; main_view
      ])
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#lifecycle">
```
```{=html}
</iframe>
```
If `Bonsai.Edge.lifecycle` is used outside of any `match%sub`s or
`Bonsai.assoc`s, `on_activate` will run once when your app starts, and
`on_deactivate` will never run.

```{=html}
```
```{=html}
<aside>
```
There's also `after_display`, which schedules a `unit Effect.t` **every
frame** while the `Bonsai.t` is active. This is almost never the best
solution for a problem.
```{=html}
</aside>
```
