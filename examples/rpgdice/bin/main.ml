open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Rpgdice = Bonsai_web_rpgdice_example

module Input_method = struct
  type t =
    | Text
    | Clicker
  [@@deriving compare, enumerate, equal, sexp, variants]

  let to_string = Variants.to_name
  let name = Source_code_position.to_string [%here]
end

module Input_method_selector = Dropdown_menu.Make (Input_method)

let input_kind ~input_method graph =
  match%sub input_method with
  | Input_method.Text ->
    String_input.component (module Rpgdice.Roll_spec) ~default_model:"" graph
  | Clicker ->
    let result_and_vdom = Dice_spec_clicker_input.component graph in
    result_and_vdom >>| Tuple2.map_fst ~f:Result.return
;;

let app graph =
  let build_result ~input ~roller ~input_method_selector =
    Vdom.Node.div
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.id "input" ] [ input_method_selector; input ]
      ; roller
      ]
  in
  let%sub input_method, input_method_selector =
    Input_method_selector.component ~default_model:Text graph
  in
  let%sub roll_spec, input = input_kind ~input_method graph in
  let roller = Roller.component roll_spec graph in
  let%arr input = input
  and roller = roller
  and input_method_selector = input_method_selector in
  build_result ~input ~roller ~input_method_selector
;;

let () = Bonsai_web.Start.start app
