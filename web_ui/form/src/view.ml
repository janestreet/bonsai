open! Core
open Bonsai_web
module Form_view = Bonsai_web_ui_form_view
include Form_view

let sexp_to_pretty_string sexp_of_t t =
  t
  |> sexp_of_t
  |> Sexp.to_string_mach
  |> String.lowercase
  |> String.map ~f:(function
    | '(' | ')' | '-' | '_' -> ' '
    | o -> o)
;;

let to_vdom_plain ?(theme = View.Expert.default_theme) ?editable view =
  View.For_components.Forms.to_vdom_plain theme ?editable view
;;

let to_vdom ?(theme = View.Expert.default_theme) ?on_submit ?(editable = `Yes_always) view
  =
  View.For_components.Forms.to_vdom theme ?on_submit ~editable view
;;

module Expert = struct
  let view_error ~theme error = View.For_components.Forms.view_error theme error

  let view_append_item ~theme append_item =
    View.For_components.Forms.append_item theme append_item
  ;;

  let view_remove_item ~theme remove_item ~index =
    View.For_components.Forms.remove_item theme remove_item ~index
  ;;
end
