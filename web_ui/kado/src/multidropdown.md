Preserved for posterity: an implementation of dropdowns that uses "size > 1" dropdowns
to implement dropdowns that have way more customizability at the expesnse of being
absolutely insane.

```ocaml
let dropdown ~constants ~input_attr ~container_attr ~title ~value ~options =
  let title =
    match title with
    | None -> Vdom.Node.none
    | Some title -> Vdom.Node.legend [ Vdom.Node.span [ Vdom.Node.text title ] ]
  in
  let colors =
    Style.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.extreme.background)
      ~border:
        (Css_gen.Color.to_string_css constants.View.Constants.extreme_primary_border)
      ~touch:(Css_gen.Color.to_string_css (`Hex "#1BA1F2"))
      ()
  in
  let onfocus =
    Vdom.Attr.on_focus (fun evt ->
      Js_of_ocaml.Js.Opt.iter evt##.target (fun target ->
        (Js_of_ocaml.Js.Unsafe.coerce target)##.size := List.length options);
      Effect.Ignore)
  in
  let onblur =
    Vdom.Attr.on_blur (fun evt ->
      Js_of_ocaml.Js.Opt.iter evt##.target (fun target ->
        (Js_of_ocaml.Js.Unsafe.coerce target)##.size := 0);
      Effect.Ignore)
  in
  let collapse select_node =
    Js_of_ocaml.Js.Opt.iter select_node (fun select_node ->
      let select_node = Js_of_ocaml.Js.Unsafe.coerce select_node in
      select_node##.size := 1)
  in
  let oninput =
    Vdom.Attr.on_keypress (fun evt ->
      (match Js_of_ocaml.Dom_html.Keyboard_code.of_event evt with
       | Enter -> print_endline "enter"
       | _ -> ());
      Js_of_ocaml.Firebug.console##log evt;
      collapse evt##.target;
      Effect.Ignore)
  in
  let onclick =
    Vdom.Attr.on_click (fun evt ->
      Js_of_ocaml.Js.Opt.iter evt##.target (fun child -> collapse child##.parentNode);
      Effect.Ignore)
  in
  Vdom.Node.fieldset
    ~attr:(Vdom.Attr.many [ Style.container; container_attr; colors ])
    [ title
    ; Vdom.Node.select
        ~attr:
          (Vdom.Attr.many
             [ Vdom.Attr.value_prop value; input_attr; onfocus; onblur; oninput ])
        (List.map options ~f:(fun text ->
           Vdom.Node.option ~attr:onclick [ Vdom.Node.text text ]))
    ]
;;
```
