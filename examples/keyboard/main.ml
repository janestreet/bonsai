open! Core
open! Bonsai_web
open Vdom_keyboard
module Js = Js_of_ocaml.Js
module Command = Keyboard_event_handler.Command

module Modifiers = struct
  type t =
    { shift : bool
    ; ctrl : bool
    ; alt : bool
    }
  [@@deriving equal, sexp]

  let of_event (event : Js_of_ocaml.Dom_html.keyboardEvent Js.t) =
    { shift = Js.to_bool event##.shiftKey
    ; ctrl = Js.to_bool event##.ctrlKey
    ; alt = Js.to_bool event##.altKey
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
        }
    | Enter_pressed
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
    let key : string option =
      event##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string
    in
    let code : string option =
      event##.code |> Js.Optdef.to_option |> Option.map ~f:Js.to_string
    in
    let keycode : int = event##.keyCode in
    let charcode : int option = event##.charCode |> Js.Optdef.to_option in
    let js_ocaml = Keyboard_code.of_event event in
    match Keyboard_event_handler.handle_event keyboard_handler event with
    | Some event -> event
    | None ->
      inject
        (Action.Something_pressed
           { js_ocaml
           ; key
           ; code
           ; keycode
           ; charcode
           ; modifiers = Modifiers.of_event event
           })
;;

let component =
  let open Bonsai.Let_syntax in
  let%sub model_and_inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:[]
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        List.append model [ action ])
  in
  return
  @@ let%pattern_map model, inject = model_and_inject in
  Vdom.Node.div
    ~attr:Vdom.Attr.(tabindex 1 @ on_keydown (handle_event inject))
    [ Vdom.Node.label
        [ Vdom.Node.text "last event"
        ; Vdom.Node.pre
            ~attr:(Vdom.Attr.style (Css_gen.color (`Name "red")))
            [ model |> List.last |> Vdom.Node.textf !"%{sexp#hum: Action.t option}" ]
        ]
    ; Vdom.Node.pre [ model |> Vdom.Node.textf !"%{sexp#hum: Action.t list}" ]
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
