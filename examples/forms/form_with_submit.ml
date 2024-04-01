open! Core
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
module E = Form.Elements

module T = struct
  type t =
    { a : int list
    ; b : string
    }
  [@@deriving sexp_of, typed_fields]

  let label_for_field = `Inferred

  let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
    fun typed_field graph ->
    match typed_field with
    | A -> E.Multiple.list (E.Textbox.int ~allow_updates_when_focused:`Never ()) graph
    | B -> E.Textbox.string ~allow_updates_when_focused:`Never () graph
  ;;
end

let form = Form.Typed.Record.make (module T)

let alert_effect =
  let alert s =
    Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string (Sexp.to_string_hum s))
  in
  Effect.of_sync_fun alert
;;

let component graph =
  let open Bonsai.Let_syntax in
  let%map form = form graph in
  let on_submit = Form.Submit.create ~f:(fun t -> alert_effect ([%sexp_of: T.t] t)) () in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Form With Submit" ]
    ; Form.view_as_vdom form ~on_submit
    ]
;;
