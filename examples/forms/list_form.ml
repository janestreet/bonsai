open! Core
open! Bonsai_web
open! Import
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module E = Form.Elements

type t =
  { a : int
  ; b : string
  }
[@@deriving sexp_of, fields]

let form_of_t =
  let%sub a = E.Textbox.string [%here] in
  let%sub b = E.Textbox.string [%here] in
  let%sub a =
    a
    >>| Form.project ~parse_exn:Int.of_string ~unparse:Int.to_string_hum
    |> Form.Dynamic.error_hint
  in
  Form.Dynamic.Record_builder.(
    build_for_record (Fields.make_creator ~a:(field a) ~b:(field b)))
;;

let inner_form =
  Form.Elements.Multiple.list
    [%here]
    form_of_t
    ~button_placement:`Indented
    ~add_element_text:(Value.return "add inner")
;;

let outer_form =
  Form.Elements.Multiple.list
    [%here]
    inner_form
    ~button_placement:`Indented
    ~add_element_text:(Value.return "add outer")
;;

let starting_value =
  [ [ { a = 5; b = "hello" }; { a = 15; b = "world" } ]
  ; [ { a = 20; b = "foo" }; { a = 11; b = "bar" }; { a = 3; b = "baz" } ]
  ]
;;

let component =
  let%sub outer_form = outer_form in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map outer_form = outer_form in
         Form.set outer_form starting_value)
      ()
  in
  return
  @@ let%map outer_form = outer_form in
  let output =
    view_t ~sexp_of:[%sexp_of: t list list Or_error.t] (Form.value outer_form)
  in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "List Form" ]
    ; Form.View.to_vdom (Form.view outer_form)
    ; output
    ]
;;
