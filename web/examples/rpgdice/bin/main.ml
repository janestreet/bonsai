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

module Model = struct
  type t =
    { input_method : Input_method.t
    ; text : Dice_spec_string_input.Model.t
    ; clicker : Dice_spec_clicker_input.Model.t
    ; roll : Roller.Model.t
    }
  [@@deriving fields]

  let init =
    { input_method = Text
    ; text = ""
    ; clicker = Dice_spec_clicker_input.initial_model
    ; roll = Roller.initial_model
    }
  ;;
end

let input : (unit, Model.t, Rpgdice.Roll_spec.t Or_error.t * Vdom.Node.t) Bonsai.t =
  let open Bonsai.Let_syntax in
  Bonsai.enum
    (module Input_method)
    ~which:(fun _input -> Model.input_method)
    ~handle:(function
      | Text ->
        Bonsai.of_module (module Dice_spec_string_input)
        |> Bonsai.Model.field Model.Fields.text
      | Clicker ->
        let%map result, vdom =
          Bonsai.of_module (module Dice_spec_clicker_input)
          |> Bonsai.Model.field Model.Fields.clicker
        in
        Ok result, vdom)
;;

let app =
  let open Bonsai.Let_syntax in
  let input =
    let%map _, input_selector =
      Bonsai.of_module (module Input_method_selector)
      |> Bonsai.Model.field Model.Fields.input_method
    and spec, input = input in
    spec, Vdom.Node.div [ Vdom.Attr.id "input" ] [ input_selector; input ]
  in
  let roller =
    Bonsai.of_module (module Roller) |> Bonsai.Model.field Model.Fields.roll
  in
  input
  >>> Bonsai.Arrow.first roller
  |> Bonsai.map ~f:(fun (input, roller) -> Vdom.Node.div [] [ input; roller ])
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    app
    ~initial_input:()
    ~initial_model:Model.init
    ~bind_to_element_with_id:"app"
;;
