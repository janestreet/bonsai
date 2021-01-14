open! Core_kernel
open! Bonsai_web
open! Import
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module E = Form.Elements
module _ = Big_form
module _ = List_form

let component =
  let%sub big_form = Big_form.component in
  let%sub list_form = List_form.component in
  return
  @@ let%map big_form = big_form
  and list_form = list_form in
  Vdom.Node.div [ Vdom.Attr.style (Css_gen.flex_container ()) ] [ big_form; list_form ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
