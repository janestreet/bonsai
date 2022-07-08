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

  let field_to_string : type a. a Typed_field.t -> string = function
    | Name -> "Name"
    | Age -> "Age"
  ;;

  let label_for_field = `Computed field_to_string

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | Name -> E.Textbox.string ()
    | Age -> E.Textbox.int ()
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

  let to_string : type a. a Typed_variant.t -> string = function
    | Unit -> "Unit"
    | Integer -> "Int"
    | Floating -> "Float"
    | Text -> "String"
    | Person -> "Person"
  ;;

  let label_for_variant = `Computed to_string

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Unit -> Bonsai.const (Form.return ())
    | Integer -> E.Textbox.int ()
    | Floating -> E.Textbox.float ()
    | Text -> E.Textbox.string ()
    | Person -> person_form
  ;;
end

let dyn_form = Form.Typed.Variant.make (module Dyn)
let radio_form = Form.Typed.Variant.make ~picker:(`Radio `Vertical) (module Dyn)

let horizontal_radio_form =
  Form.Typed.Variant.make ~picker:(`Radio `Horizontal) (module Dyn)
;;

let component =
  let%map.Computation person = person_form
  and dyn = dyn_form
  and radio_form = radio_form
  and horizontal_radio_form = horizontal_radio_form in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Typed Fields" ]
    ; Form.View.to_vdom (Form.view person)
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants" ]
    ; Form.View.to_vdom (Form.view (Form.label "dynamic value" dyn))
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants (Radio)" ]
    ; Form.View.to_vdom (Form.view (Form.label "radio!" radio_form))
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants (Horizontal Radio)" ]
    ; Form.View.to_vdom (Form.view (Form.label "horizontal radio!" horizontal_radio_form))
    ]
;;
