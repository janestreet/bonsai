open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Underlying_codemirror = Codemirror
module Codemirror = Bonsai_web_ui_codemirror

module Style =
  [%css
    stylesheet
      {|
  body {
    font-size: 18px;
    line-height: 1.5em;
  }

  fieldset {
    background: white;
    max-height: 500px;
    overflow: auto;
    padding: 1em;
    border-radius: 3px;
    border: 1px solid #858585;

    display: flex;
    align-items: flex-start;
  }

  fieldset legend {
    color: rgb(60,60,60);
    padding: 0 0.5em;
  }

  .output_html button {
    color: #2196f3;
    background:none;
    border:0;
    font-size:inherit;
    font-family:inherit;
    cursor: pointer;
  }

  .output {
    background: repeating-conic-gradient(rgb(240,240,240) 0% 25%, white 0% 50%) 50% / 20px 20px;
    display:flex;
    align-items:center;
    justify-content:center;
  }

  .hbox {
    display:flex;
  }

  pre {
    margin:0;
  }

  p {
    line-height: 1.5em;
    margin-top: 0.3em;
    margin-bottom: 0.3;
  }

  h2 {
    margin-bottom:0;
    margin-top:1.5em;
  }

  body {
    font-family: sans-serif;
    background: rgb(250, 250, 250);
  }

  html, body {
    margin: 0;
    padding: 0;
    box-sizing:border-box;
  }

  .section {
    display:flex;
    flex-direction: column;
    flex-wrap: wrap;
    height:100%;
    column-gap: 2em;
  }

  .app {
    display: flex;
    flex-direction: column;
    gap: 3em;
    max-width:min(100vw, 1000px);
    padding: 2em;
  }

  .code_and_output {
    display:flex;
    flex-direction:column;
    align-items:flex-start;
    min-width: 900px;
    max-width: min-content;
  }

  .code_and_output > * {
      min-width:600px;
  }
|}]

module type Demo = sig
  val name : string
  val description : string
  val view : (Vdom.Node.t * string) Computation.t
  val selector : string option
  val filter_attrs : (string -> string -> bool) option
end

let make_sections sections =
  let%sub app =
    List.map sections ~f:(fun (section_title, description, subsections) ->
      let%sub subsections = Bonsai.Computation.all subsections in
      let%arr subsections = subsections in
      Vdom.Node.div
        [ Vdom.Node.h1 [ Vdom.Node.text section_title ]
        ; Vdom.Node.p [ Vdom.Node.text description ]
        ; Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.section) subsections
        ])
    |> Bonsai.Computation.all
  in
  let%arr app = app in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.app) app
;;

let make_demo (module M : Demo) =
  let filter_printed_attributes = M.filter_attrs in
  let pick_interesting_nodes =
    match M.selector with
    | None -> fun node -> [ node ]
    | Some selector ->
      fun node -> Virtual_dom_test_helpers.Node_helpers.select ~selector node
  in
  let%sub codemirror =
    let open Underlying_codemirror in
    let with_conversion_of_bool b =
      With_conversion.create ~t_to_js:Gen_js_api.Ojs.bool_to_js b
    in
    let extensions =
      [ Gutter.highlight_active_line_gutter ()
      ; History.history (History.Config.create ())
      ; State.Facet.of_
          State.Editor_state.allow_multiple_selections
          (with_conversion_of_bool true)
      ; Highlight.syntax_highlighting
          ~options:(Highlight.Syntax_highlighting_options.create ~fallback:true ())
          Highlight.default_highlight_style
          ()
      ; State.Facet.of_ View.Editor_view.editable (with_conversion_of_bool false)
      ; View.Editor_view.line_wrapping
      ; Mllike.ocaml
        |> Stream_parser.Stream_language.define
        |> Stream_parser.Stream_language.to_language
        |> Language.extension
      ]
    in
    Codemirror.of_initial_state
      ~name:"codemirror for demo"
      (State.Editor_state.create (State.Editor_state_config.create ~extensions ()))
  in
  let%sub view, demo = M.view in
  let%sub () =
    Bonsai.Edge.on_change
      (module String)
      demo
      ~callback:
        (let%map codemirror = codemirror in
         fun demo -> Codemirror.set_lines codemirror (String.split_lines demo))
  in
  let%arr codemirror = codemirror
  and view = view in
  let html_output =
    view
    |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
    |> pick_interesting_nodes
    |> List.map
         ~f:
           (Virtual_dom_test_helpers.Node_helpers.to_string_html
              ?filter_printed_attributes)
    |> String.concat ~sep:"\n"
  in
  let legend = Vdom.Node.create "legend" [ Vdom.Node.text "HTML" ] in
  let ocaml_code =
    Vdom.Node.fieldset
      [ Vdom.Node.create "legend" [ Vdom.Node.text "OCaml" ]; codemirror.view ]
  in
  let view_as_vdom =
    Vdom.Node.fieldset
      ~attr:(Vdom.Attr.class_ Style.output)
      [ Vdom.Node.create "legend" [ Vdom.Node.text "Rendered" ]; view ]
  in
  let html_output =
    Vdom.Node.fieldset
      ~attr:(Vdom.Attr.class_ Style.output_html)
      [ legend; Vdom.Node.pre [ Vdom.Node.text html_output ] ]
  in
  Vdom.Node.div
    [ Vdom.Node.h2 [ Vdom.Node.text M.name ]
    ; Vdom.Node.p [ Vdom.Node.text M.description ]
    ; Vdom.Node.div
        [ Vdom.Node.div
            ~attr:(Vdom.Attr.class_ Style.code_and_output)
            [ ocaml_code; view_as_vdom; html_output ]
        ]
    ]
;;
