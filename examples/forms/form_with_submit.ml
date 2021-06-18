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

let form =
  let%sub a = E.Textbox.int [%here] in
  let%sub b = E.Textbox.string [%here] in
  let%sub t =
    Form.Dynamic.Record_builder.(
      build_for_record (Fields.make_creator ~a:(field a) ~b:(field b)))
  in
  return (t >>| Form.label "my record")
;;

let alert_effect =
  let eff =
    unstage
      (Effect.of_sync_fun (fun s ->
         Js_of_ocaml.Dom_html.window##alert
           (Js_of_ocaml.Js.string (Sexp.to_string_hum s))))
  in
  fun s -> eff s |> Effect.inject_ignoring_response
;;

let component =
  let%map.Computation form = form in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Form With Submit" ]
    ; Form.view_as_vdom
        form
        ~on_submit:(Form.Submit.create ~f:(fun t -> alert_effect ([%sexp_of: t] t)) ())
    ]
;;
