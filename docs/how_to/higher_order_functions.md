# Higher-Order Functions

In regular OCaml, higher-order functions like `List.fold`, the `|>`
pipeline operator, and `match` statements enable control flow and
abstraction.

The `match%sub` and `Bonsai.assoc` [control flow
primitives](../guide/05-control_flow.mdx) are higher-order functions
too! And we can use them to build higher-order abstractions like:

-   A generic query renderer, which dispatches an RPC to fetch some
    data, and uses `match%sub` to display a loading indicator while the
    fetch is in progress, and some
    `local_ graph -> Vdom.Node.t Bonsai.t` when the data is present.
-   A generic table/list, where every row/cell can have its own state,
    implemented with `assoc`.
-   A modal, where you'd use `match%sub` to only render some content if
    the open state is `true`.

These will have type signatures like:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val higher_order : ('a Bonsai.t -> local_ graph -> 'b Bonsai.t) -> ... -> local_ graph -> 'c Bonsai.t
```

For example, here's how we'd build a simple modal:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/higher_order_examples.ml,part=hoc_modal -->
```
``` ocaml
type t =
  { view : Vdom.Node.t Bonsai.t
  ; open_modal : unit Effect.t Bonsai.t
  }

let modal
      ~(title : Vdom.Node.t Bonsai.t)
      ~(content : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      (local_ graph)
  : t
  =
  let is_open, set_is_open = Bonsai.state false graph in
  let open_modal =
    let%arr set_is_open = set_is_open in
    set_is_open true
  in
  let view =
    match%sub is_open with
    | false -> Bonsai.return Vdom.Node.none
    | true ->
      (* only instantiate [content] here in the [true] branch *)
      let%arr content = content graph
      and title = title
      and set_is_open = set_is_open in
      let close_button =
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_is_open false) ]
          [ Vdom.Node.text "X" ]
      in
      Vdom.Node.div
        ~attrs:
          [ [%css
            {|
            position: fixed;
            top: 0; bottom: 0; left: 0; right: 0;
            height: fit-content; width: fit-content;
            margin: auto;
            border: 1px solid black;
            background-color: white;|}]
          ]
        [ Vdom.Node.h1 [ title; close_button ]; content ]
  in
  { view; open_modal }
;;
```

And here's how we could use the modal, with content that instantiates
state:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/higher_order_examples.ml,part=modal_example -->
```
``` ocaml
let modal_example (local_ graph) =
  let title = Bonsai.return (Vdom.Node.text "Hi there!") in
  let content (local_ graph) =
    let count, set_count = Bonsai.state 0 graph in
    let on_activate =
      let%arr count = count
      and set_count = set_count in
      set_count (count + 1)
    in
    let () = Bonsai.Edge.lifecycle ~on_activate graph in
    let%arr count = count in
    Vdom.Node.div
      [ Vdom.Node.text [%string "This modal has been opened %{count#Int} times..."] ]
  in
  let { view = modal_view; open_modal } = modal ~title ~content graph in
  let%arr modal_view = modal_view
  and open_modal = open_modal in
  Vdom.Node.div
    ~attrs:[ [%css {|height: 400px;|}] ]
    [ modal_view
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> open_modal) ]
        [ Vdom.Node.text "open modal" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#modal_example">
```
```{=html}
</iframe>
```
Note that state is retained, and [lifecycle effects](./lifecycles.mdx)
run on every open / close.
