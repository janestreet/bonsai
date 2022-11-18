open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Underlying_codemirror = Codemirror
module Codemirror = Bonsai_web_ui_codemirror
module Form = Bonsai_web_ui_form

module type Demo = sig
  val name : string
  val description : string
  val view : (Vdom.Node.t * string) Computation.t
  val selector : string option
  val filter_attrs : (string -> string -> bool) option
end

let if_empty_then_none constructor text =
  if String.is_empty (String.strip text)
  then Vdom.Node.None
  else constructor [ Vdom.Node.text text ]
;;

let make_sections ~theme_picker sections =
  let%sub theme = View.Theme.current in
  let%sub app =
    List.map sections ~f:(fun (section_title, description, subsections) ->
      let%sub subsections = Bonsai.Computation.all subsections in
      let%arr subsections = subsections in
      Vdom.Node.div
        [ if_empty_then_none Vdom.Node.h1 section_title
        ; if_empty_then_none Vdom.Node.p description
        ; Vdom.Node.div ~attr:Style.section subsections
        ])
    |> Bonsai.Computation.all
  in
  let%arr app = app
  and theme_picker = theme_picker
  and theme = theme in
  Vdom.Node.div
    ~attr:
      (Vdom.Attr.many
         [ Style.app
         ; Style.Variables.set
             ~bg:(Css_gen.Color.to_string_css (View.primary_colors theme).background)
             ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
             ~extreme_primary_border:
               (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
             ~extreme_bg:
               (Css_gen.Color.to_string_css (View.extreme_colors theme).background)
             ()
         ])
    [ theme_picker; Vdom.Node.div ~attr:Style.container app ]
;;

let codemirror ~language ~content =
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
    ; language
    ]
  in
  let%sub codemirror =
    let%sub codemirror_theme =
      let%map.Computation theme = Bonsai_web.View.Theme.current in
      Bonsai_web.View.For_components.Codemirror.theme theme
    in
    Codemirror.with_dynamic_extensions
      (module struct
        type t = Codemirror_themes.t option [@@deriving equal, sexp]
      end)
      ~name:"codemirror for demo"
      codemirror_theme
      ~compute_extensions:
        (Value.return (fun theme_opt ->
           let extra_extensions =
             match theme_opt with
             | None -> []
             | Some theme -> [ Codemirror_themes.get theme ]
           in
           extra_extensions @ extensions))
      ~initial_state:(State.Editor_state.create (State.Editor_state_config.create ()))
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module String)
      content
      ~callback:
        (let%map codemirror = codemirror in
         fun demo -> Codemirror.set_lines codemirror (String.split_lines demo))
  in
  return codemirror
;;

let make_demo (module M : Demo) =
  let filter_printed_attributes k v =
    (not (String.equal k "custom-css-vars"))
    && (Option.value M.filter_attrs ~default:(fun _ _ -> true)) k v
  in
  let pick_interesting_nodes =
    match M.selector with
    | None -> fun node -> [ node ]
    | Some selector ->
      fun node -> Virtual_dom_test_helpers.Node_helpers.select ~selector node
  in
  let%sub view, demo = M.view in
  let%sub ocaml_codemirror =
    codemirror
      ~content:demo
      ~language:
        Underlying_codemirror.(
          Mllike.ocaml
          |> Stream_parser.Stream_language.define
          |> Stream_parser.Stream_language.to_language
          |> Language.extension)
  in
  let%sub rendered_or_html =
    Form.Elements.Radio_buttons.enumerable
      ~init:`Rendered
      ~layout:`Horizontal
      ~extra_attrs:(Value.return [ Style.rendred_or_html_picker ])
      (module struct
        type t =
          [ `Rendered
          | `Html
          ]
        [@@deriving sexp, equal, compare, enumerate]
      end)
  in
  let%sub display_which =
    match%sub rendered_or_html >>| Form.value_or_default ~default:`Rendered with
    | `Rendered -> return view
    | `Html ->
      let%sub html_codemirror =
        let%sub content =
          let%arr view = view in
          view
          |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
          |> pick_interesting_nodes
          |> List.map
               ~f:
                 (Virtual_dom_test_helpers.Node_helpers.to_string_html
                    ~filter_printed_attributes)
          |> String.concat ~sep:"\n"
        in
        codemirror
          ~content
          ~language:Underlying_codemirror.(Lang_html.html () |> Language.extension)
      in
      let%arr html_codemirror = html_codemirror in
      html_codemirror.view
  in
  let%arr ocaml_codemirror = ocaml_codemirror
  and display_which = display_which
  and rendered_or_html = rendered_or_html in
  let toggler = rendered_or_html |> Form.view |> Form.View.to_vdom_plain in
  let ocaml_code =
    Vdom.Node.fieldset
      [ Vdom.Node.create "legend" [ Vdom.Node.text "OCaml" ]; ocaml_codemirror.view ]
  in
  let output =
    Vdom.Node.fieldset
      ~attr:Style.output
      [ Vdom.Node.create "legend" toggler; display_which ]
  in
  Vdom.Node.div
    [ if_empty_then_none Vdom.Node.h2 M.name
    ; if_empty_then_none Vdom.Node.p M.description
    ; ocaml_code
    ; output
    ]
;;
