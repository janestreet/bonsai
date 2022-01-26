open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form
module E = Form.Elements

module Person = struct
  type t =
    { name : string
    ; age : int
    }
  [@@deriving sexp_of, typed_fields]

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | Name -> E.Textbox.string [%here]
    | Age -> E.Textbox.int [%here]
  ;;
end

let person_form = Form.Typed.Record.make (module Person)

module Dyn = struct
  type t =
    | Unit
    | Integer of int
    | Floating of float
    | Text of string
    | Person of Person.t
  [@@deriving sexp_of, typed_variants]

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Unit -> Bonsai.const (Form.return ())
    | Integer -> E.Textbox.int [%here]
    | Floating -> E.Textbox.float [%here]
    | Text -> E.Textbox.string [%here]
    | Person -> person_form
  ;;
end

let dyn_form = Form.Typed.Variant.make (module Dyn)

let component =
  let%map.Computation person = person_form
  and dyn = dyn_form in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Typed Fields" ]
    ; Form.View.to_vdom (Form.view person)
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants" ]
    ; Form.View.to_vdom (Form.view (Form.label "dynamic value" dyn))
    ]
;;
