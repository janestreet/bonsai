open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view
module Auto_generated = Bonsai_web_ui_auto_generated

let get_vdom form =
  match Form.View.to_vdom_plain (Form.view form) with
  | [ v ] -> v
  | other -> Vdom.Node.div other
;;

let get_vdom_detailed form = Form.view_as_vdom form

let form_result_spec (type a) ?filter_printed_attributes ?(get_vdom = get_vdom) sexp_of_a
  : (module Result_spec.S with type t = a Form.t and type incoming = a)
  =
  (module struct
    type t = a Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes Fn.id) in
      let vdom = get_vdom form in
      let vdom = V.view vdom in
      let value =
        Form.value form
        |> [%sexp_of: a Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming = Form.set
  end)
;;
