open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Codemirror
open Virtual_dom
module Form = Bonsai_web_ui_form.With_automatic_view
module Codemirror = Bonsai_web_ui_codemirror

(* Make the codemirror editor take up most of the view *)
let () =
  Inline_css.Private.append
    {|
  .cm-editor {
    height: 80vh;
  }

  label {
    font-weight: normal;
  }
|}
;;

module Fruit_sexp_grammar_auto_complete = struct
  module Fruit = struct
    type t =
      | Apple
      | Blueberry
      | Banana
      | Pineapple
      | Custom of string
    [@@deriving sexp, equal, sexp_grammar]
  end

  module Query = struct
    type t = Fruit.t Blang.t [@@deriving sexp, equal, sexp_grammar]
  end

  let codemirror_editor =
    Codemirror.with_sexp_grammar_autocompletion
      ~include_non_exhaustive_hint:false
      ~extra_extension:
        (State.Extension.of_list
           [ Basic_setup.basic_setup; Codemirror_rainbow_parentheses.extension () ])
      (Bonsai.return Query.t_sexp_grammar)
  ;;
end

module Ocaml_syntax_highlighting = struct
  let doc =
    {|open! Core

(* Syntax highlight for ocaml *)

let x = List.map [ 1; 2; 3; 4; 5 ] ~f:(fun x -> x + 1)

let y =
  let z = 3 in
  let a = 4 in
  z + a
;;
|}
  ;;

  let codemirror_editor ~theme =
    let create_extensions state =
      let theme = Codemirror_themes.get state in
      [ Basic_setup.basic_setup
      ; Mllike.ocaml
        |> Stream_parser.Stream_language.define
        |> Stream_parser.Stream_language.to_language
        |> Language.extension
      ; theme
      ]
    in
    let create_state extensions =
      State.Editor_state.create (State.Editor_state_config.create ~doc ~extensions ())
    in
    Codemirror.with_dynamic_extensions
      (module Codemirror_themes)
      ~equal:[%equal: Codemirror_themes.t]
      ~initial_state:(create_state (create_extensions Codemirror_themes.Material_dark))
      ~compute_extensions:(Bonsai.return create_extensions)
      theme
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

module Diff_syntax_highlighting = struct
  let doc =
    {|diff --git a/index.html b/index.html
index c1d9156..7764744 100644
--- a/index.html
+++ b/index.html
@@ -95,7 +95,8 @@ StringStream.prototype = {
     <script>
       var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
         lineNumbers: true,
-        autoMatchBrackets: true
+        autoMatchBrackets: true,
+        onGutterClick: function(x){console.log(x);}
       });
     </script>
   </body>
diff --git a/lib/codemirror.js b/lib/codemirror.js
index 04646a9..9a39cc7 100644
--- a/lib/codemirror.js
+++ b/lib/codemirror.js
@@ -1420,6 +1425,7 @@ var CodeMirror = (function() {
     readOnly: false,
     onChange: null,
     onCursorActivity: null,
+    onGutterClick: null,
     autoMatchBrackets: false,
     workTime: 200,
     workDelay: 300,
    |}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup
              ; Diff.diff
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Html_syntax_highlighting = struct
  let doc =
    {|<html>
  <head>
    <title>My website!</title>
  </head>
  <div>
    Hello!
  </div>
</html>
  |}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup; Lang_html.html () |> Language.extension ]
            ()))
  ;;
end

module Css_syntax_highlighting = struct
  let doc =
    {|:root {
    margin: 0;
}

html {
  background-color: #ffc700;
  font-family: "Verdana", sans-serif;
}
|}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:[ Basic_setup.basic_setup; Lang_css.css () |> Language.extension ]
            ()))
  ;;
end

module Javascript_syntax_highlighting = struct
  let doc =
    {|const http = require('http');

const hostname = '127.0.0.1';
const port = 3000;

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello World');
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});|}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup
              ; Lang_javascript.javascript () |> Language.extension
              ]
            ()))
  ;;
end

module Php_syntax_highlighting = struct
  let doc = {|<?php
echo "Hello World!";
?>|}

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:[ Basic_setup.basic_setup; Lang_php.php () |> Language.extension ]
            ()))
  ;;
end

module Rust_syntax_highlighting = struct
  let doc =
    {|use yew::prelude::*;

#[function_component(App)]
fn app() -> Html {
    html! {
        <h1>{ "Hello World" }</h1>
    }
}

fn main() {
    yew::start_app::<App>();
}|}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup; Lang_rust.rust () |> Language.extension ]
            ()))
  ;;
end

module Common_lisp_syntax_highlighting = struct
  let doc =
    {|(in-package :cl-postgres)

;; These are used to synthesize reader and writer names for integer
;; reading/writing functions when the amount of bytes and the
;; signedness is known. Both the macro that creates the functions and
;; some macros that use them create names this way.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun integer-reader-name (bytes signed)
    (intern (with-standard-io-syntax
              (format nil "~a~a~a~a" '#:read- (if signed "" '#:u) '#:int bytes))))
  (defun integer-writer-name (bytes signed)
    (intern (with-standard-io-syntax
              (format nil "~a~a~a~a" '#:write- (if signed "" '#:u) '#:int bytes))))) |}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup
              ; Commonlisp.common_lisp
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Scheme_syntax_highlighting = struct
  let doc =
    {|;; Building a list of squares from 0 to 9:
;; Note: loop is simply an arbitrary symbol used as a label. Any symbol will do.

(define (list-of-squares n)
  (let loop ((i n) (res '()))
    (if (< i 0)
        res
        (loop (- i 1) (cons (* i i) res)))))|}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:
              [ Basic_setup.basic_setup
              ; Scheme.scheme
                |> Stream_parser.Stream_language.define
                |> Stream_parser.Stream_language.to_language
                |> Language.extension
              ]
            ()))
  ;;
end

module Xml_syntax_highlighting = struct
  let doc =
    {|<?xml version="1.0" encoding="ISO-8859-1" ?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"></xs:schema>|}
  ;;

  let codemirror_editor =
    Codemirror.of_initial_state
      (State.Editor_state.create
         (State.Editor_state_config.create
            ~doc
            ~extensions:[ Basic_setup.basic_setup; Lang_xml.xml () |> Language.extension ]
            ()))
  ;;
end

module Which_language = struct
  type t =
    (* Fruit is the blang language defined above *)
    | Ocaml
    | Fruit
    | Fsharp
    | Markdown
    | Sml
    | Sql
    | Common_lisp
    | Scheme
    | Diff
    | Html
    | Css
    | Javascript
    | Php
    | Rust
    | Xml
  [@@deriving enumerate, sexp, equal, compare]

  let to_string = function
    | Fruit -> "Fruit-based blang with autocomplete"
    | Fsharp -> "F# syntax highlighting"
    | Markdown -> "Markdown syntax highlighting"
    | Ocaml -> "OCaml syntax highlighting"
    | Sml -> "SML syntax highlighting"
    | Sql -> "SQL syntax highlighting"
    | Common_lisp -> "Common Lisp syntax highlighting"
    | Scheme -> "Scheme syntax highlighting"
    | Diff -> "Diff syntax highlighting"
    | Html -> "HTML syntax highlighting"
    | Css -> "CSS syntax highlighting"
    | Javascript -> "JavaScript syntax highlighting"
    | Php -> "PHP syntax highlighting"
    | Rust -> "Rust syntax highlighting"
    | Xml -> "Xml syntax highlighting"
  ;;
end

let no_theme_picker x =
  let%arr x = x in
  None, x
;;

let component graph =
  let language_picker =
    Form.Elements.Dropdown.enumerable
      ~to_string:Which_language.to_string
      (module Which_language)
      graph
  in
  let chosen_language =
    let%arr language_picker = language_picker in
    Form.value language_picker |> Or_error.ok_exn
  in
  let%sub theme_picker, codemirror =
    (* Note: [Codemirror.with_dynamic_extensions] is generally preferred to [match%sub]ing
       and choosing a codemirror editor instance. For the purposes of this demo, the code
       is optimized for showing off the ease with which people can create different
       codemirror editors, so we do the less-preferred option. *)
    match%sub chosen_language with
    | Which_language.Fruit ->
      no_theme_picker
        (Fruit_sexp_grammar_auto_complete.codemirror_editor ~name:"fruit" graph)
    | Fsharp ->
      no_theme_picker (Fsharp_syntax_highlighting.codemirror_editor ~name:"fsharp" graph)
    | Markdown ->
      no_theme_picker
        (Markdown_syntax_highlighting.codemirror_editor ~name:"markdown" graph)
    | Ocaml ->
      let theme_picker =
        Form.Elements.Dropdown.enumerable
          ~to_string:Codemirror_themes.to_string
          (module Codemirror_themes)
          graph
        |> Bonsai.map ~f:(Form.label "theme")
      in
      let chosen_theme =
        let%arr theme_picker = theme_picker in
        Form.value theme_picker |> Or_error.ok_exn
      in
      let c =
        Ocaml_syntax_highlighting.codemirror_editor
          ~name:"ocaml"
          ~theme:chosen_theme
          graph
      in
      let%arr c = c
      and theme_picker = theme_picker in
      Some theme_picker, c
    | Sml ->
      no_theme_picker @@ Sml_syntax_highlighting.codemirror_editor ~name:"sml" graph
    | Sql ->
      no_theme_picker @@ Sql_syntax_highlighting.codemirror_editor ~name:"sql" graph
    | Common_lisp ->
      no_theme_picker
      @@ Common_lisp_syntax_highlighting.codemirror_editor ~name:"common lisp" graph
    | Scheme ->
      no_theme_picker @@ Scheme_syntax_highlighting.codemirror_editor ~name:"scheme" graph
    | Diff ->
      no_theme_picker @@ Diff_syntax_highlighting.codemirror_editor ~name:"diff" graph
    | Html ->
      no_theme_picker @@ Html_syntax_highlighting.codemirror_editor ~name:"html" graph
    | Css ->
      no_theme_picker @@ Css_syntax_highlighting.codemirror_editor ~name:"css" graph
    | Javascript ->
      no_theme_picker
      @@ Javascript_syntax_highlighting.codemirror_editor ~name:"javascript" graph
    | Php ->
      no_theme_picker @@ Php_syntax_highlighting.codemirror_editor ~name:"php" graph
    | Rust ->
      no_theme_picker @@ Rust_syntax_highlighting.codemirror_editor ~name:"rust" graph
    | Xml ->
      no_theme_picker @@ Xml_syntax_highlighting.codemirror_editor ~name:"xml" graph
  in
  let codemirror_view =
    let%arr codemirror = codemirror in
    Codemirror.view codemirror
  in
  let%arr codemirror_view = codemirror_view
  and language_picker = language_picker
  and theme_picker = theme_picker in
  Vdom.Node.div
    [ Vdom.Node.text "Choose your editor extension:"
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.style (Css_gen.flex_container ~direction:`Row ()) ]
        [ Form.view_as_vdom language_picker
        ; Option.value_map ~default:Vdom.Node.none ~f:Form.view_as_vdom theme_picker
        ]
    ; codemirror_view
    ]
;;

let () = Bonsai_web.Start.start component
