open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form
module E = Form.Elements

module T = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving sexp_of, typed_fields]

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | A -> E.Textbox.int [%here]
    | B -> E.Textbox.string [%here]
  ;;
end

let form = Form.Typed.Record.make (module T)

let alert_effect =
  let alert s =
    Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string (Sexp.to_string_hum s))
  in
  Effect.of_sync_fun alert
;;

let component =
  let%map.Computation form = form in
  let on_submit = Form.Submit.create ~f:(fun t -> alert_effect ([%sexp_of: T.t] t)) () in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Form With Submit" ]
    ; Form.view_as_vdom form ~on_submit
    ]
;;
