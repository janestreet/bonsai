open! Core
open Bonsai_web
module Bonsai_form = Bonsai_form_experimental

module A_B_or_C = struct
  type t =
    | A
    | B
    | C
  [@@deriving enumerate, equal, sexp, variants]

  let to_string = Variants.to_name
end

module A_B_or_C_or_all = struct
  type t =
    | All
    | Just of A_B_or_C.t
  [@@deriving enumerate, equal, sexp]

  let to_string = function
    | All -> "All"
    | Just abc -> A_B_or_C.to_string abc
  ;;
end

module Result = struct
  type t =
    { string : string
    ; date : Date.t option
    ; date_from_string : Date.t
    ; sexp_from_string : Sexp.t
    ; bool_from_checkbox : bool
    ; bool_from_dropdown : bool
    ; string_option : string option
    ; a_b_or_c_or_all : A_B_or_C_or_all.t
    }
  [@@deriving fields ~getters ~iterators:create, sexp]
end

let or_all_dropdown =
  Bonsai_form.Dropdown.of_enum
    (module A_B_or_C_or_all)
    ~default:(A_B_or_C_or_all.Just A_B_or_C.A)
;;

let validated_print_error_next_to_it component ~sexp_of_t =
  let%map_open.Bonsai_form.Combine ({ value; view } : _ Bonsai_form.Product.With_view.t) =
    component
  in
  let view =
    Vdom.Node.div
      [ view
      ; Vdom.Node.pre
          [ value |> [%sexp_of: t Or_error.t] |> Sexp.to_string_hum |> Vdom.Node.text ]
      ]
  in
  Bonsai_form.Product.With_view.create ~value ~view
;;

module type M = sig
  type t [@@deriving sexp_of]

  include Stringable with type t := t
end

let string_validated (type t) (module M : M with type t = t) =
  Bonsai_form.Validated.make_via_string (module M) (Bonsai_form.text_input ~default:"")
  |> validated_print_error_next_to_it ~sexp_of_t:[%sexp_of: M.t]
;;

let form =
  let open Bonsai.Arrow_deprecated.Infix in
  let%map_open.Bonsai_form.Combine { value = string; view = text_input } =
    Bonsai_form.text_input ~default:"" |> lift ~f:Result.string
  and { value = date; view = date_input } =
    Bonsai_form.date_picker ~default:(Some (Date.of_string "2020-01-01"))
    |> lift ~f:Result.date
  and { value = date_from_string; view = date_from_string_input } =
    string_validated (module Date) |> lift ~f:Result.date_from_string
  and { value = sexp_from_string; view = sexp_from_string_input } =
    string_validated (module Sexp) |> lift ~f:Result.sexp_from_string
  and { value = bool_from_checkbox; view = bool_from_checkbox_input } =
    Bonsai_form.checkbox_input ?label:None ~default:true ()
    |> lift ~f:Result.bool_from_checkbox
  and { value = bool_from_dropdown; view = bool_from_dropdown_input } =
    Bonsai_form.Dropdown.of_enum (module Bool) ~default:true
    |> lift ~f:Result.bool_from_dropdown
  and { value = string_option; view = string_option_view } =
    const [ "First"; "Second"; "Third" ]
    @>> Bonsai_form.Dropdown.of_input_opt (module String) ~default:None
    |> lift ~f:Result.string_option
  and { value = a_b_or_c_or_all; view = a_b_or_c_or_all_input } =
    or_all_dropdown |> lift ~f:Result.a_b_or_c_or_all
  in
  let result =
    let%map.Or_error date_from_string = date_from_string
    and sexp_from_string = sexp_from_string in
    Result.Fields.create
      ~string
      ~date
      ~date_from_string
      ~sexp_from_string
      ~bool_from_checkbox
      ~bool_from_dropdown
      ~string_option
      ~a_b_or_c_or_all
  in
  let view =
    let f name node = Vdom.Node.(tr [ td [ text name ]; td [ node ] ]) in
    Vdom.Node.table
      [ Vdom.Node.tbody
          [ f "string" text_input
          ; f "date" date_input
          ; f "string validated date" date_from_string_input
          ; f "string validated sexp" sexp_from_string_input
          ; f "boolean checkbox" bool_from_checkbox_input
          ; f "boolean dropdown" bool_from_dropdown_input
          ; f "optional dropdown" string_option_view
          ; f "a_b_or_c_or_all" a_b_or_c_or_all_input
          ]
      ]
  in
  Bonsai_form.Product.With_view.create ~value:result ~view
;;

let component =
  let%map_open.Bonsai.Arrow_deprecated { value = { value = result; view }; set = _ } =
    form
  in
  let result_readback =
    Vdom.Node.div
      [ Vdom.Node.h2 [ Vdom.Node.text "Result" ]
      ; Vdom.Node.code
          [ Vdom.Node.pre
              [ Vdom.Node.text (Sexp.to_string_hum [%sexp (result : Result.t Or_error.t)])
              ]
          ]
      ]
  in
  Vdom_layout.vbox [ view; result_readback ]
;;

let (_ : _ Arrow_deprecated.Start.Handle.t) =
  Arrow_deprecated.Start.start_standalone
    ~initial_input:()
    ~bind_to_element_with_id:"app"
    component
;;
