open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Codemirror
open Virtual_dom
module Form = Bonsai_web_ui_form
module Codemirror = Bonsai_web_ui_codemirror

(* Make the codemirror editor take up most of the view *)
let () = Inline_css.Private.append {|
  .cm-editor {
    height: 80vh;
  }
|}

module Fruit_sexp_grammar_auto_complete = struct
  module Fruit = struct
    type t =
      | Apple
      | Blueberry
      | Banana
      | Pineapple
    [@@deriving sexp, equal, sexp_grammar]
  end

  module Query = struct
    type t = Fruit.t Blang.t [@@deriving sexp, equal, sexp_grammar]
  end

  let codemirror_editor =
    Codemirror.with_sexp_grammar_autocompletion (Value.return Query.t_sexp_grammar)
  ;;
end

module Ocaml_syntax_highlighting = struct
  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:
              [ Basic_setup.basic_setup
              ; Mllike.ocaml
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Fsharp_syntax_highlighting = struct
  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:
              [ Basic_setup.basic_setup
              ; Mllike.fsharp
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Sml_syntax_highlighting = struct
  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:
              [ Basic_setup.basic_setup
              ; Mllike.sml
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Markdown_syntax_highlighting = struct
  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:
              [ Basic_setup.basic_setup; Lang_markdown.markdown () |> Language.extension ]
            ()))
  ;;
end

module Sql_syntax_highlighting = struct
  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~extensions:[ Basic_setup.basic_setup; Lang_sql.sql () |> Language.extension ]
            ()))
  ;;
end

module Which_language = struct
  type t =
    (* Fruit is the blang language defined above *)
    | Fruit
    | Fsharp
    | Markdown
    | Ocaml
    | Sml
    | Sql
  [@@deriving enumerate, sexp, equal, compare]

  let to_string = function
    | Fruit -> "Fruit-based blang with autocomplete"
    | Fsharp -> "F# syntax highlighting"
    | Markdown -> "Markdown syntax highlighting"
    | Ocaml -> "OCaml syntax highlighting"
    | Sml -> "SML syntax highlighting"
    | Sql -> "SQL syntax highlighting"
  ;;
end

let component =
  let%sub language_picker =
    Form.Elements.Dropdown.enumerable
      ~to_string:Which_language.to_string
      (module Which_language)
  in
  let%sub chosen_language =
    let%arr language_picker = language_picker in
    Form.value language_picker |> Or_error.ok_exn
  in
  let%sub { view = codemirror_view; _ } =
    (* Note: [Codemirror.with_dynamic_extensions] is generally preferred to [match%sub]ing
       and choosing a codemirror editor instance. For the purposes of this demo, the code
       is optimized for showing off the ease with which people can create different
       codemirror editors, so we do the less-preferred option. *)
    match%sub chosen_language with
    | Which_language.Fruit ->
      Fruit_sexp_grammar_auto_complete.codemirror_editor ~name:"fruit"
    | Fsharp -> Fsharp_syntax_highlighting.codemirror_editor ~name:"fsharp"
    | Markdown -> Markdown_syntax_highlighting.codemirror_editor ~name:"markdown"
    | Ocaml -> Ocaml_syntax_highlighting.codemirror_editor ~name:"ocaml"
    | Sml -> Sml_syntax_highlighting.codemirror_editor ~name:"sml"
    | Sql -> Sql_syntax_highlighting.codemirror_editor ~name:"sql"
  in
  let%arr codemirror_view = codemirror_view
  and language_picker = language_picker in
  Vdom.Node.div
    [ Vdom.Node.text "Choose your editor extension:"
    ; Form.view_as_vdom language_picker
    ; codemirror_view
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
