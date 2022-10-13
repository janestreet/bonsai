open! Core
open Bonsai_web
module Attr = Vdom.Attr
module Node = Vdom.Node
open Bonsai_web_ui_form.View.Private

module Tooltip = struct
  module Style =
    [%css.raw
      {|
        .container {
          position: relative;
          display: inline-block;
        }
        .content {
          white-space: pre-line;
          visibility: hidden;
          width: 300px;
          background-color: beige;
          text-align: center;
          border-radius: 3px;
          padding: 0.5em 1em 0.5em 1em;
          border: 1px solid black;
          position: absolute;
          z-index: 1;
          left: 100%;
          cursor: text;
        }
        .container:hover .content {
          visibility: visible;
        }
 |}]

  let wrap ?tooltip_element ~attr children =
    Node.div
      ~attr:Attr.(class_ Style.container @ attr)
      [ children
      ; (match tooltip_element with
         | None -> Node.none
         | Some element -> Node.div ~attr:Attr.(class_ Style.content) [ element ])
      ]
  ;;
end

module Style =
  [%css.raw
    {|
      .form {
        --font-size: 12px;
        --font-family: monospace;
        font-family: var(--font-family);
        font-size: var(--font-size);
      }

      .form table {
        width: 100%;
      }
      .form input,.form select, .form textarea {
        font-family: var(--font-family);
        font-size: var(--font-size);
        border:none;
        border-bottom:1px solid gray;
        padding-bottom:1px;
        width:100%;
        background-color:inherit;
      }
      .form input, .form textarea {
        min-width:250px;
      }
      .form textarea {
        height:1.4em;
        resize:none;
      }
      .form textarea:hover {
        resize:both;
      }
      .form select {
        min-width:100px;
      }
      .form input:focus-visible,.form select:focus-visible {
        outline:none;
        border:none;
        border-bottom:2px solid black;
        padding-bottom:0px;
      }
      .form button {
        font-family: var(--font-family);
        font-size: var(--font-size);
        cursor: pointer;
        color: blue;
        background: none;
        padding-top: 0.1rem;
        padding-bottom: 0.1rem;
      }
      .form button:hover, .form button:focus-visible {
        border-bottom: 1px solid blue !important;
        margin-bottom: -1px;
      }
      fieldset[disabled] .form button {
        display:none;
      }
      .label {
        font-weight:bold;
        padding-right: 2px;
        text-align: left;
        user-select: none;
        white-space: nowrap;
        display: flex;
        justify-content: space-between;
      }
      .label_error {
        text-decoration: underline wavy red;
        cursor: pointer;
      }
      .label_info {
        cursor: pointer;
      }
      .label_info::after {
        content:"";
        width: 0;
        height: 0;
        border-style: solid;
        border-width: 0 6px 6px 0;
        border-color: transparent #007bff transparent transparent;
        display:block;
      }
      .clear_fieldset_styles {
        border: 0;
        margin: 0;
        padding: 0;
      }
      .mod_depth_1 {
        --accent-h:209;
        --accent-s:100%;
        --accent-l:50%;
      }
      .mod_depth_2 {
        --accent-h:137;
        --accent-s:100%;
        --accent-l:36%;
      }
      .mod_depth_3 {
        --accent-h:32;
        --accent-s:100%;
        --accent-l:49%;
      }
      .mod_depth_4 {
        --accent-h:0;
        --accent-s:81%;
        --accent-l:54%;
      }
      .nested_table {
        padding-left:1.3rem;
        border-width: 0 0 0 1px;
        border-color: hsla(var(--accent-h), var(--accent-s), var(--accent-l), 1);
        border-style: solid;
        background-color: hsla(var(--accent-h), var(--accent-s), 95%, 1);
      }
      .nested_table:hover {
        border-width: 0 0 0 2px;
        margin-left: -1px;
      }
|}]

(* These CSS rules are used to clear user-agent styles. We use :where
   to decrease specificity (otherwise child elements would not be able to
   easily set the same properties with their own classes)

   We need to append the CSS because [ppx_css] does not mangle classes in the pseudo
   selector*)
let () =
  let form = Style.form in
  Inline_css.Private.append
    [%string
      {|
      :where(.%{form}) *,
      :where(.%{form}) *::before,
      :where(.%{form}) *::after {
        cursor:pointer;
        box-sizing: border-box;
        margin: 0;
        padding: 0;
        border: none;
        outline: none;
      }
|}]
;;

let nested_table_depth_classes =
  Style.[ mod_depth_1; mod_depth_2; mod_depth_3; mod_depth_4 ]
;;

let nested_table depth children =
  let table_attr =
    Attr.classes
      [ List.nth_exn
          nested_table_depth_classes
          (depth mod List.length nested_table_depth_classes)
      ; Style.nested_table
      ]
  in
  Node.tr [ Node.td ~attr:(Attr.colspan 100) [ Node.table ~attr:table_attr children ] ]
;;

let label_wrapper ?(child = Node.text "") ?(attr = Attr.empty) ?tooltip ?error () =
  let error =
    Option.map error ~f:(fun { Error_details.error; _ } ->
      Node.text (Error.to_string_hum error))
  in
  let label_classes =
    List.filter_opt
      [ Some Style.label
      ; Option.some_if (Option.is_some error) Style.label_error
      ; Option.some_if (Option.is_some tooltip) Style.label_info
      ]
  in
  let tooltip_element =
    match List.filter_opt [ error; tooltip ] with
    | [] -> None
    | elements -> Some (Node.div (List.intersperse elements ~sep:(Node.hr ())))
  in
  Node.td
    [ Tooltip.wrap ?tooltip_element ~attr:Attr.(classes label_classes @ attr) child ]
;;

let rec to_vdom ~depth = function
  | Empty -> []
  | Group { label; tooltip; view; error } ->
    let rest = to_vdom view ~depth:(depth + 1) in
    let header_is_inhabited =
      Option.is_some label || Option.is_some tooltip || Option.is_some error
    in
    if header_is_inhabited
    then (
      let label = label_wrapper ?child:label ?tooltip ?error ~attr:Attr.(colspan 2) () in
      [ Node.tr [ label ]; nested_table depth rest ])
    else rest
  | Header_group { label; tooltip; view; header_view; error } ->
    let rest = to_vdom view ~depth:(depth + 1) in
    let header_view =
      let colspan = if Option.is_some label then Attr.empty else Attr.colspan 2 in
      let nodes, key = to_vdom_plain header_view in
      Node.td ?key ~attr:colspan nodes
    in
    let label = label_wrapper ?child:label ?tooltip ?error () in
    [ Node.tr [ label; header_view ]; nested_table depth rest ]
  | Submit_button _ as btn ->
    let button, key = to_vdom_plain btn in
    [ Node.tr ?key button ]
  | Row { label; tooltip; id; form; error } ->
    let label =
      match label with
      | Some label ->
        (* <label> nodes can be clicked on to focus the input element contained
           inside.  By setting display:block, even the whitespace to the right
           of the label is clickable, meaning that mis-clicking on particularly
           small labels is less likely. *)
        Node.label
          ~attr:
            (Attr.many_without_merge
               [ Attr.for_ id; Attr.style (Css_gen.display `Block) ])
          [ label ]
      | _ -> Node.text ""
    in
    [ (* This key prevents inputs of different "kinds" from clobbering each other *)
      Node.tr ~key:id [ label_wrapper ?tooltip ?error ~child:label (); Node.td [ form ] ]
    ]
  | List l -> List.concat_map l ~f:(to_vdom ~depth)

(* If the form is just a single row, return the view for it without wrapping *)
and to_vdom_plain = function
  | Empty -> [], None
  | Header_group { label = _; tooltip = _; header_view; view; error = _ } ->
    let header, _key = to_vdom_plain header_view in
    let body, _key = to_vdom_plain view in
    header @ body, None
  | Group { label = _; tooltip = _; view; error = _ } ->
    let vdom, _key = to_vdom_plain view in
    vdom, None
  | Row { label = _; tooltip = _; id; form; error = _ } -> [ form ], Some id
  | List l -> List.concat_map l ~f:(fun t -> to_vdom_plain t |> fst), None
  | Submit_button { on_submit; text; attr } ->
    let nodes =
      match on_submit with
      | Some event ->
        let event = Vdom.Effect.(Many [ event; Prevent_default; Stop_propagation ]) in
        [ Node.button
            ~attr:(Attr.combine attr (Attr.on_click (fun _ -> event)))
            [ Vdom.Node.text text ]
        ]
      | None ->
        [ Node.button ~attr:(Attr.combine attr Attr.disabled) [ Vdom.Node.text text ] ]
    in
    nodes, None
;;

let to_vdom ?on_submit ?(editable = `Yes_always) view =
  let view =
    match on_submit with
    | Some { on_submit; button_text = Some button_text; handle_enter = _; button_attr } ->
      let button = Submit_button { text = button_text; on_submit; attr = button_attr } in
      concat view button
    | _ -> view
  in
  let root_table =
    Node.table ~attr:Attr.(class_ Style.form) [ Node.tbody (to_vdom view ~depth:0) ]
  in
  let root_table =
    match editable with
    | `Yes_always -> root_table
    | `Currently_yes -> with_fieldset ~currently_editable:true root_table
    | `Currently_no -> with_fieldset ~currently_editable:false root_table
  in
  match on_submit with
  | Some { on_submit; handle_enter = true; _ } ->
    let always_use = [ Vdom.Effect.Prevent_default; Vdom.Effect.Stop_propagation ] in
    let event =
      match on_submit with
      | None -> Vdom.Effect.Many always_use
      | Some event -> Vdom.Effect.Many (event :: always_use)
    in
    Node.create "form" ~attr:Attr.(on_submit (fun _ -> event)) [ root_table ]
  | _ -> root_table
;;
