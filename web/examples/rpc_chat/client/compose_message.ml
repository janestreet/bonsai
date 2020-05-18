open! Core_kernel
open! Async_kernel
open! Bonsai_web

module T = struct
  let name = "compose component"

  module Input = struct
    type t = { send_message : string -> unit Effect.t } [@@deriving fields]

    let create = Fields.create
  end

  module Model = struct
    type t = { message : string } [@@deriving equal, sexp]

    let default = { message = "" }
  end

  module Result = Vdom.Node

  module Action = struct
    type t = Update of string [@@deriving sexp_of]
  end

  let apply_action ~inject:_ ~schedule_event:_ _input _model (Action.Update message) =
    { Model.message }
  ;;

  let compute ~(inject : Action.t -> _) (input : Input.t) (model : Model.t) =
    let on_ret =
      let is_key_ret key =
        String.equal
          "Enter"
          (key##.code
           |> Js_of_ocaml.Js.Optdef.to_option
           |> Option.value_exn
           |> Js_of_ocaml.Js.to_string)
      in
      Vdom.Attr.on_keypress (fun key ->
        if is_key_ret key
        then
          Vdom.Event.Many
            [ Effect.inject_ignoring_response (input.send_message model.message)
            ; inject (Update "")
            ]
        else Vdom.Event.Ignore)
    in
    let on_input = Vdom.Attr.on_input (fun _ s -> inject (Update s)) in
    let value = Vdom.Attr.string_property "value" model.message in
    let text_input =
      Vdom.Node.input [ on_ret; on_input; value ] [ Vdom.Node.text "submit" ]
    in
    let submit_button =
      Vdom_input_widgets.Button.simple "send" ~on_click:(fun _ ->
        Vdom.Event.Many
          [ Effect.inject_ignoring_response (input.send_message model.message)
          ; inject (Update "")
          ])
    in
    Vdom.Node.div [ Vdom.Attr.id "compose" ] [ text_input; submit_button ]
  ;;
end

include T

let component = Bonsai.of_module (module T) ~default_model:Model.default
