open! Core
open Bonsai_web
module Attr = Vdom.Attr
module Form = Bonsai_web_ui_form.With_automatic_view
module Node = Vdom.Node
module Form_view = Form.View

module Tooltip = struct
  module Style =
  [%css
  stylesheet
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
      ~attrs:[ Attr.(Style.container @ attr) ]
      [ children
      ; (match tooltip_element with
         | None -> Node.none
         | Some element -> Node.div ~attrs:[ Style.content ] [ element ])
      ]
  ;;
end

module Style =
[%css
stylesheet
  ~rewrite:
    [ "--font-family", "--font-family"
    ; "--font-size", "--font-size"
    ; "--accent-h", "--accent-h"
    ; "--accent-s", "--accent-s"
    ; "--accent-l", "--accent-l"
    ]
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
        --accent-h:0;
        --accent-s:81%;
        --accent-l:54%;
      }
      .mod_depth_2 {
        --accent-h:209;
        --accent-s:100%;
        --accent-l:50%;
      }
      .mod_depth_3 {
        --accent-h:137;
        --accent-s:100%;
        --accent-l:36%;
      }
      .mod_depth_4 {
        --accent-h:32;
        --accent-s:100%;
        --accent-l:49%;
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
  let form = Style.For_referencing.form in
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

let nested_table eval_context children =
  let table_attr =
    Attr.many
      [ List.nth_exn
          nested_table_depth_classes
          (View.Expert.Form_context.depth eval_context
           mod List.length nested_table_depth_classes)
      ; Style.nested_table
      ]
  in
  Node.tr
    [ Node.td ~attrs:[ Attr.colspan 100 ] [ Node.table ~attrs:[ table_attr ] children ] ]
;;

let label_wrapper ?(attr = Attr.empty) (context : Form_view.context) =
  let error =
    Option.map context.error ~f:(fun error -> Node.text (Error.to_string_hum error))
  in
  let label = Option.value context.label ~default:(Node.text "") in
  let label_classes =
    List.filter_opt
      [ Some Style.label
      ; Option.some_if (Option.is_some context.error) Style.label_error
      ; Option.some_if (Option.is_some context.tooltip) Style.label_info
      ]
  in
  let tooltip_element =
    match List.filter_opt [ error; context.tooltip ] with
    | [] -> None
    | elements -> Some (Node.div (List.intersperse elements ~sep:(Node.hr ())))
  in
  Node.td [ Tooltip.wrap ?tooltip_element ~attr:Attr.(many label_classes @ attr) label ]
;;

let header_is_inhabited view_context =
  Option.is_some view_context.Form_view.label
  || Option.is_some view_context.tooltip
  || Option.is_some view_context.error
;;

let with_auto_generated_forms ~theme =
  let module Form_context = View.Expert.Form_context in
  View.Expert.override_theme theme ~f:(fun (module M) ->
    (module struct
      class c =
        object (self)
          inherit M.c
          method! theme_name = "Bonsai_web_ui_auto_generated"

          method! form_tuple ~eval_context ~view_context ts =
            let header_is_inhabited = header_is_inhabited view_context in
            let eval_context =
              if header_is_inhabited
              then Form_context.incr_depth eval_context
              else eval_context
            in
            let rest = List.concat_map ts ~f:(self#form_view ~eval_context) in
            if header_is_inhabited
            then (
              let label = label_wrapper ~attr:Attr.(colspan 2) view_context in
              [ Node.tr [ label ]; nested_table eval_context rest ])
            else rest

          method! form_raw
            ~eval_context
            ~view_context
            ({ unique_key; raw_view } : Form_view.raw) =
            let view_context =
              { view_context with
                label =
                  Option.map view_context.label ~f:(fun label ->
                    Node.label
                      ~attrs:[ Attr.for_ unique_key; Attr.style (Css_gen.display `Block) ]
                      [ label ])
              }
            in
            [ Node.tr
                ~key:unique_key
                [ label_wrapper view_context
                ; Node.td
                    [ raw_view view_context ~editable:(Form_context.editable eval_context)
                    ]
                ]
            ]

          method! form_record ~eval_context ~view_context fields =
            let header_is_inhabited = header_is_inhabited view_context in
            let eval_context =
              if header_is_inhabited
              then Form_context.incr_depth eval_context
              else eval_context
            in
            let rest =
              List.concat_map fields ~f:(fun { field_name; field_view } ->
                self#form_view
                  ~eval_context
                  (Form_view.suggest_label field_name field_view))
            in
            if header_is_inhabited
            then (
              let label = label_wrapper ~attr:Attr.(colspan 2) view_context in
              [ Node.tr [ label ]; nested_table eval_context rest ])
            else rest

          method! form_variant
            ~eval_context
            ~view_context
            ({ clause_selector; selected_clause } : Form_view.variant) =
            let eval_context = Form_context.incr_depth eval_context in
            let rest =
              match selected_clause with
              | None -> []
              | Some { clause_name = _; clause_view } ->
                self#form_view ~eval_context clause_view
            in
            let label = label_wrapper view_context in
            [ Node.tr [ label; Node.td [ clause_selector ] ]
            ; nested_table eval_context rest
            ]

          method! form_option
            ~eval_context
            ~view_context
            ({ toggle; status } : Form_view.option_view) =
            let eval_context = Form_context.incr_depth eval_context in
            let rest =
              match status with
              | Currently_none None -> []
              | Currently_some t | Currently_none (Some t) ->
                self#form_view ~eval_context t
            in
            let label = label_wrapper view_context in
            [ Node.tr [ label; Node.td [ toggle ] ]; nested_table eval_context rest ]

          method! form_list
            ~eval_context
            ~view_context
            ({ list_items; append_item; legacy_button_position = _ } :
              Form_view.list_view) =
            let header_is_inhabited = header_is_inhabited view_context in
            let eval_context =
              if header_is_inhabited
              then Form_context.incr_depth eval_context
              else eval_context
            in
            let rest =
              let items_and_removals =
                List.concat_mapi list_items ~f:(fun i { item_view; remove_item } ->
                  let eval_context = Form_context.incr_depth eval_context in
                  let rest = self#form_view ~eval_context item_view in
                  let remove_button =
                    self#form_remove_item ~eval_context remove_item ~index:i
                  in
                  [ Node.tr
                      [ Node.td
                          ~attrs:
                            [ Attr.colspan 2; Attr.style (Css_gen.font_weight `Bold) ]
                          [ remove_button ]
                      ]
                  ; nested_table eval_context rest
                  ])
              in
              let append_item =
                Node.tr
                  [ Node.td
                      ~attrs:[ Vdom.Attr.colspan 2; Style.label ]
                      [ self#form_append_item ~eval_context append_item ]
                  ]
              in
              items_and_removals @ [ append_item ]
            in
            if header_is_inhabited
            then (
              let label = label_wrapper ~attr:Attr.(colspan 2) view_context in
              [ Node.tr [ label ]; nested_table eval_context rest ])
            else rest

          method! form_toplevel_combine rows =
            Node.table ~attrs:[ Style.form ] [ Node.tbody rows ]
        end
    end))
;;

let to_vdom ?(theme = View.Expert.default_theme) ?on_submit ?editable view =
  Vdom.Node.lazy_
    (lazy
      (Form.View.to_vdom
         ?on_submit
         ?editable
         view
         ~theme:(with_auto_generated_forms ~theme)))
;;
