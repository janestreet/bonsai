open! Core_kernel
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
    >>| Form.label "custom label for \"a\""
    |> Form.Dynamic.error_hint
  in
  Form.Dynamic.Record_builder.(
    build_for_record (Fields.make_creator ~a:(field a) ~b:(field b)))
;;

let form = Form.Elements.Multiple.list [%here] form_of_t

let starting_value =
  [ { a = 5; b = "hello" }; { a = 10; b = "there" }; { a = 15; b = "world" } ]
;;

let component =
  let%sub form = form in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map form = form in
         Form.set form starting_value)
      ()
  in
  return
  @@ let%map form = form in
  let output = view_t ~sexp_of:[%sexp_of: t list Or_error.t] (Form.value form) in
  Vdom.Node.div
    []
    [ Vdom.Node.h1 [] [ Vdom.Node.text "List Form" ]
    ; Form.View.to_vdom (Form.view form)
    ; output
    ]
;;
