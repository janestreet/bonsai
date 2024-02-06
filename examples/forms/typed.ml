open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_automatic_view
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
    | Name -> E.Textbox.string ~allow_updates_when_focused:`Never ()
    | Age -> E.Textbox.int ~allow_updates_when_focused:`Never ()
  ;;
end

let person_form = Form.Typed.Record.make (module Person)

module Dyn = struct
  type t =
    | Unit
    | Integer of int
    | Floating of float
    | Text of string
    | People of Person.t list
  [@@deriving sexp_of, typed_variants]

  let to_string : type a. a Typed_variant.t -> string = function
    | Unit -> "Unit"
    | Integer -> "Int"
    | Floating -> "Float"
    | Text -> "String"
    | People -> "People"
  ;;

  let label_for_variant = `Computed to_string
  let initial_choice = `First_constructor

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Unit -> Bonsai.const (Form.return ())
    | Integer -> E.Textbox.int ~allow_updates_when_focused:`Never ()
    | Floating -> E.Textbox.float ~allow_updates_when_focused:`Never ()
    | Text -> E.Textbox.string ~allow_updates_when_focused:`Never ()
    | People -> Form.Typed.Record.make_table (module Person)
  ;;
end

let dyn_form = Form.Typed.Variant.make (module Dyn)
let radio_form = Form.Typed.Variant.make ~picker:(`Radio `Vertical) (module Dyn)

let horizontal_radio_form =
  Form.Typed.Variant.make ~picker:(`Radio `Horizontal) (module Dyn)
;;

module Food = struct
  module Meal = struct
    type t =
      | Breakfast
      | Lunch
      | Dinner
    [@@deriving enumerate, sexp_of, compare]
  end

  module T = struct
    type t =
      | Snack
      | Meal of Meal.t
    [@@deriving sexp_of, compare, typed_variants]
  end

  include T

  let to_string : type a. a Typed_variant.t -> string = function
    | Snack -> "Snack"
    | Meal -> "Meal"
  ;;

  let sexp_of_variant_argument =
    `Custom
      (fun (type a) (variant : a Typed_variant.t) : (a -> Sexp.t) ->
        (match variant with
         | Snack -> Unit.sexp_of_t
         | Meal -> Meal.sexp_of_t))
  ;;

  let label_for_variant = `Computed to_string

  let form_for_variant
    : type a cmp.
      a Typed_variant.t
      -> (a, cmp) Bonsai.comparator
      -> (a, cmp) Set.t Form.t Computation.t
    =
    fun variant (module Cmp) ->
    match variant with
    | Snack ->
      let%sub.Bonsai checkbox = E.Checkbox.bool ~default:false () in
      let%arr.Bonsai checkbox = checkbox in
      Form.project
        checkbox
        ~parse_exn:(fun is_set ->
          if is_set then Set.singleton (module Cmp) () else Set.empty (module Cmp))
        ~unparse:(fun set -> Set.is_empty set |> not)
    | Meal -> E.Typeahead.set (module Cmp) ~all_options:(Value.return Meal.all)
  ;;

  include Comparable.Make_plain (T)
end

let food_form = Form.Typed.Variant.make_set (module Food)

let component =
  let%map.Computation person = person_form
  and dyn = dyn_form
  and radio_form = radio_form
  and horizontal_radio_form = horizontal_radio_form
  and food_form = food_form in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Typed Fields" ]
    ; Form.View.to_vdom (Form.view person)
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants" ]
    ; Form.View.to_vdom (Form.view (Form.label "dynamic value" dyn))
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants (Radio)" ]
    ; Form.View.to_vdom (Form.view (Form.label "radio!" radio_form))
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants (Horizontal Radio)" ]
    ; Form.View.to_vdom (Form.view (Form.label "horizontal radio!" horizontal_radio_form))
    ; Vdom.Node.h1 [ Vdom.Node.text "Typed Variants (Set of Values)" ]
    ; Form.View.to_vdom (Form.view food_form)
    ; Vdom.Node.pre
        [ Form.value food_form
          |> [%sexp_of: Food.Set.t Or_error.t]
          |> Sexp.to_string_hum
          |> Vdom.Node.text
        ]
    ]
;;
