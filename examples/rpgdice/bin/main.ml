open! Core_kernel
open! Async_kernel
open! Import

module Input_method = struct
  type t =
    | Text
    | Clicker
  [@@deriving compare, enumerate, equal, sexp, variants]

  let to_string = Variants.to_name
  let name = Source_code_position.to_string [%here]
end

module Input_method_selector = Dropdown_menu.Make (Input_method)

let input_kind ~input_method =
  let open Bonsai.Let_syntax in
  match%sub input_method with
  | Input_method.Text ->
    String_input.component (module Rpgdice.Roll_spec) ~default_model:""
  | Clicker ->
    let%sub result_and_vdom = Dice_spec_clicker_input.component in
    return (result_and_vdom >>| Tuple2.map_fst ~f:Result.return)
;;

let app =
  let build_result ~input ~roller ~input_method_selector =
    Vdom.Node.div
      []
      [ Vdom.Node.div [ Vdom.Attr.id "input" ] [ input_method_selector; input ]; roller ]
  in
  let open Bonsai.Let_syntax in
  let%sub input_method, input_method_selector =
    Input_method_selector.component ~default_model:Text
  in
  let%sub roll_spec, input = input_kind ~input_method in
  let%sub roller = Roller.component roll_spec in
  return
    (let%map input = input
     and roller = roller
     and input_method_selector = input_method_selector in
     build_result ~input ~roller ~input_method_selector)
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
