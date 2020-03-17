open! Core_kernel
open! Async_kernel
open! Import
include String_input_intf

module Make (Conv : Conv) = struct
  module Input = Unit
  module Model = String

  module Action = struct
    type t = Set_input of string [@@deriving sexp_of]
  end

  module Result = struct
    type t = Conv.t Or_error.t * Vdom.Node.t
  end

  let apply_action ~inject:_ ~schedule_event:_ _input _model (action : Action.t) =
    match action with
    | Set_input raw_input -> raw_input
  ;;

  let compute ~inject _input (model : Model.t) =
    let conv = Or_error.try_with (fun () -> Conv.of_string model) in
    let textbox =
      Vdom.Node.input
        ([ Vdom.Attr.type_ "text"
         ; Vdom.Attr.on_input (fun _ contents -> inject (Action.Set_input contents))
         ; Vdom.Attr.value model
         ]
         @
         match conv with
         | Ok _ -> []
         | Error _ -> [ Vdom.Attr.class_ "invalid" ])
        []
    in
    let conv_display =
      let attrs, text =
        match conv with
        | Error err -> [ Vdom.Attr.class_ "invalid" ], Error.to_string_hum err
        | Ok spec -> [], Conv.to_string_hum spec
      in
      Vdom.Node.pre attrs [ Vdom.Node.text text ]
    in
    conv, Vdom.Node.div [] [ textbox; conv_display ]
  ;;

  let name = Conv.name
end
