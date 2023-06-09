open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Vdom_keyboard
module Js = Js_of_ocaml.Js
module Command = Keyboard_event_handler.Command

module Modifiers = struct
  type t =
    { shift : bool
    ; ctrl : bool
    ; alt : bool
    ; alt_graph : bool
    ; meta : bool
    }
  [@@deriving equal, sexp]

  let of_event (event : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t) =
    { shift = Js.to_bool event##.shiftKey
    ; ctrl = Js.to_bool event##.ctrlKey
    ; alt = Js.to_bool event##.altKey
    ; alt_graph = Js.to_bool (event##getModifierState (Js.string "AltGraph"))
    ; meta = Js.to_bool event##.metaKey
    }
  ;;
end

module Action = struct
  type t =
    | Something_pressed of
        { key : string option
        ; code : string option
        ; keycode : int
        ; charcode : int option
        ; modifiers : Modifiers.t
        ; js_ocaml : Keyboard_code.t
        ; keystroke : Keystroke.t
        }
    | Enter_pressed
    | Ctrl_alt_e_pressed
    | Ctrl_shift_slash_pressed
    | Down_arrow_pressed
  [@@deriving equal, sexp]
end

module Model = struct
  type t = Action.t list [@@deriving sexp, equal]
end

let handle_event inject =
  let keyboard_handler =
    Keyboard_event_handler.of_command_list_exn
      [ { Command.keys = [ Keystroke.create' ~ctrl:() ~shift:() Keyboard_code.Slash ]
        ; description = "slash"
        ; group = None
        ; handler = (fun _ -> inject Action.Ctrl_shift_slash_pressed)
        }
      ; { Command.keys = [ Keystroke.create' ~ctrl:() ~alt:() Keyboard_code.KeyE ]
        ; description = "ctrl+alt+e"
        ; group = None
        ; handler = (fun _ -> inject Action.Ctrl_alt_e_pressed)
        }
      ; { Command.keys = [ Keystroke.create' Keyboard_code.Enter ]
        ; description = "enter"
        ; group = None
        ; handler = (fun _ -> inject Action.Enter_pressed)
        }
      ; { Command.keys = [ Keystroke.create' Keyboard_code.ArrowDown ]
        ; description = "arrow-down"
        ; group = None
        ; handler = (fun _ -> inject Action.Down_arrow_pressed)
        }
      ]
  in
  fun event ->
    let key = event##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
    let code = event##.code |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
    let keycode = event##.keyCode in
    let charcode = event##.charCode |> Js.Optdef.to_option in
    let js_ocaml = Keyboard_code.of_event event in
    let specialized =
      match Keyboard_event_handler.handle_event keyboard_handler event with
      | Some event -> event
      | None -> Ui_effect.Ignore
    in
    let something_pressed =
      inject
        (Action.Something_pressed
           { js_ocaml
           ; key
           ; code
           ; keycode
           ; charcode
           ; modifiers = Modifiers.of_event event
           ; keystroke = Keystroke.of_event event
           })
    in
    Effect.Many [ something_pressed; specialized ]
;;

module Style = [%css stylesheet {| .red { color: red } |}]

let component =
  let%sub model_and_inject =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:[]
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        List.append model [ action ])
  in
  let%arr model, inject = model_and_inject in
  let last_event = List.last model in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.on_keydown (handle_event inject) ]
    [ Vdom.Node.div
        ~attrs:[ Style.red ]
        [ Vdom.Node.sexp_for_debugging
            [%sexp "Last Event", (last_event : Action.t option)]
        ]
    ; Vdom.Node.sexp_for_debugging [%sexp (model : Action.t list)]
    ]
;;

let () = Bonsai_web.Start.start component
