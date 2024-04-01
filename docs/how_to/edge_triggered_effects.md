# Edge-Triggered Effects

`Bonsai.Edge.on_change'` allows us to run some dynamically-computed
`Effect.t` whenever some `'a Bonsai.t` changes, and when it is first
calculated:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/edge_triggered_examples.ml,part=on_change -->
```
``` ocaml
let on_change_demo (local_ graph) =
  let view, value = State_examples.counter ~step:(Bonsai.return 1) graph in
  Bonsai.Edge.on_change'
    ~equal:Int.equal
    ~callback:
      (Bonsai.return (fun (prev_value : int option) (new_value : int) ->
         match prev_value with
         | None -> (* Do nothing on first render*) Effect.Ignore
         | Some prev_value ->
           Effect.alert
             [%string "prev value: %{prev_value#Int}, new value: %{new_value#Int}"]))
    value
    graph;
  view
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#on_change">
```
```{=html}
</iframe>
```
The `~equal` argument allows us to define what "change" means.

Another category of edge-triggering is the `on_activate` and
`on_deactivate` [lifecycles](./lifecycles.mdx).

### Downsides to Using `Edge`

Declarative programs are easy to reason about and test. Extensive use of
the `Edge` module will make your program less and less declarative. As a
general rule, you should try to avoid using `Edge` if other, more
declarative solutions exist.
