open! Core_kernel
open! Async_kernel
open! Import

module Dice_spec_string_input = String_input.Make (struct
    include Rpgdice.Roll_spec

    let name = Source_code_position.to_string [%here]
  end)

module Input_method = struct
  module T = struct
    type t =
      | Text
      | Clicker
    [@@deriving compare, enumerate, equal, sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let name = Source_code_position.to_string [%here]
end

module Input_method_selector = Dropdown_menu.Make (Input_method)

let input : (Input_method.t, Rpgdice.Roll_spec.t Or_error.t * Vdom.Node.t) Bonsai.t =
  let open Bonsai.Let_syntax in
  Bonsai.enum
    (module Input_method)
    ~which:Fn.id
    ~handle:(function
      | Text ->
        ignore @>> Bonsai.of_module (module Dice_spec_string_input) ~default_model:""
      | Clicker ->
        let%map result, vdom =
          ignore
          @>> Bonsai.of_module
                (module Dice_spec_clicker_input)
                ~default_model:Dice_spec_clicker_input.initial_model
        in
        Ok result, vdom)
;;

let input_method_and_input =
  let open Bonsai.Arrow in
  let open Bonsai.Let_syntax in
  let%map (spec, input), input_selector =
    Bonsai.of_module (module Input_method_selector) ~default_model:Input_method.Text
    >>> first input
  in
  spec, Vdom.Node.div [ Vdom.Attr.id "input" ] [ input_selector; input ]
;;

let app =
  let open Bonsai.Let_syntax in
  let roller = Bonsai.of_module (module Roller) ~default_model:Roller.initial_model in
  input_method_and_input
  >>> Bonsai.Arrow.first roller
  |> Bonsai.map ~f:(fun (input, roller) -> Vdom.Node.div [] [ input; roller ])
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone app ~initial_input:() ~bind_to_element_with_id:"app"
;;
