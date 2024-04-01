# Effects and Stale Values

Let's say we have some state, which is used to calculate some
`Bonsai.t`. This calculation might:

1.  Combine the input with other inputs
2.  Send an RPC based on the input, and return the result
3.  Do some expensive and complicated incremental transformations

How do we build an effect that sets the state, and then does something
with the result of step 3 (the expensive transformation)? Let's try!

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_stale_examples.ml,part=stale_closed_naive -->
```
``` ocaml
let set_and_run_effect_naive (other_input : string Bonsai.t) (local_ graph) =
  let count, set_state = Bonsai.state 0 graph in
  let computed = complicated_transformation other_input count in
  let set_and_alert =
    let%arr computed = computed
    and set_state = set_state in
    fun new_state ->
      let%bind.Effect () = set_state new_state in
      Effect.alert computed
  in
  let%arr count = count
  and set_and_alert = set_and_alert in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_and_alert (count + 1)) ]
        [ Vdom.Node.text "increment count" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#stale_closed_naive">
```
```{=html}
</iframe>
```
Looks great! Except that it's wrong. The alert shows the *old* value of
`computed`.

Why? When we click the button, we schedule the `set_and_alert` effect.
That effect closes over the value of `computed` from before the button
was clicked. So even though `computed` is recalculated when
`set_state new_state` runs, we are stuck with an outdated `computed`.

What we'd really like to do is "peek" at the current value of `computed`
instead of having to close over it. Cue `Bonsai.peek`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=peek -->
```
``` ocaml
val peek : 'a Bonsai.t -> local_ Bonsai.graph -> 'a Computation_status.t Effect.t Bonsai.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/effect_stale_examples.ml,part=stale_closed_peek -->
```
``` ocaml
let set_and_run_effect_peek (other_input : string Bonsai.t) (local_ graph) =
  let count, set_state = Bonsai.state 0 graph in
  let computed = complicated_transformation other_input count in
  let peek_computed = Bonsai.peek computed graph in
  let set_and_alert =
    let%arr peek_computed = peek_computed
    and set_state = set_state in
    fun new_state ->
      let%bind.Effect () = set_state new_state in
      match%bind.Effect peek_computed with
      | Active computed -> Effect.alert computed
      | Inactive -> Effect.Ignore
  in
  let%arr count = count
  and set_and_alert = set_and_alert in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_and_alert (count + 1)) ]
        [ Vdom.Node.text "increment count" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#stale_closed_peek">
```
```{=html}
</iframe>
```
If the `peek` backing an effect is in an [inactive code
branch](./lifecycles.mdx), it will be unable to retrieve a fresh value,
so it returns values wrapped in a `Computation_status.t`, just like
[`state_machine1`'s `input`](../guide/04-state.mdx).
