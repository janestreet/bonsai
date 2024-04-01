open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Vdom_keyboard

module Css =
[%css
stylesheet
  {|
  .block {
    position: absolute;
    width: 200px;
    height: 200px;
    background: green;
    display: flex;
    justify-content: center;
    align-items: center;
    text-align: center;
    color: white;
  }

  .help_container {
    position: absolute;
    background: black;
    color: white;
    font-family: monospace;
    max-width: 500px;
    margin-left: auto;
    margin-right: auto;
    top: 0;
    left: 0;
    right: 0;
  }
  |}]

let offset_state_machine graph =
  Tuple2.uncurry Bonsai.both
  @@ Bonsai.state_machine0
       graph
       ~sexp_of_model:[%sexp_of: Int.t]
       ~equal:[%equal: Int.t]
       ~sexp_of_action:[%sexp_of: Int.t]
       ~default_model:400
       ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) x delta -> x + delta)
;;

let component graph =
  let%sub x, add_x = offset_state_machine graph in
  let%sub y, add_y = offset_state_machine graph in
  let show_help, set_show_help =
    Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
  in
  let handler =
    let%arr add_x = add_x
    and add_y = add_y
    and set_show_help = set_show_help in
    let command ?cond ~keys ~description f =
      let handler =
        let open Keyboard_event_handler.Handler in
        match cond with
        | None -> with_prevent_default f
        | Some cond -> only_handle_if cond f ~prevent_default:()
      in
      { Keyboard_event_handler.Command.keys; description; group = None; handler }
    in
    let open Keyboard_event_handler.Condition in
    let key = Keystroke.create' in
    let is_not_text_input = not_ has_text_input_target in
    Keyboard_event_handler.of_command_list_exn
      [ (* Small movement *)
        command
          ~description:"Move block left by 5 pixels"
          ~keys:[ key KeyH; key ArrowLeft ]
          ~cond:is_not_text_input
          (fun _ev -> add_x (-5))
      ; command
          ~description:"Move block down by 5 pixels"
          ~keys:[ key KeyJ; key ArrowDown ]
          ~cond:is_not_text_input
          (fun _ev -> add_y 5)
      ; command
          ~description:"Move block up by 5 pixels"
          ~keys:[ key KeyK; key ArrowUp ]
          ~cond:is_not_text_input
          (fun _ev -> add_y (-5))
      ; command
          ~description:"Move block right by 5 pixels"
          ~keys:[ key KeyL; key ArrowRight ]
          ~cond:is_not_text_input
          (fun _ev -> add_x 5)
        (* Big movement *)
      ; command
          ~description:"Move block left by 20 pixels"
          ~keys:[ key ~shift:() KeyH; key ~shift:() ArrowLeft ]
          ~cond:is_not_text_input
          (fun _ev -> add_x (-20))
      ; command
          ~description:"Move block down by 20 pixels"
          ~keys:[ key ~shift:() KeyJ; key ~shift:() ArrowDown ]
          ~cond:is_not_text_input
          (fun _ev -> add_y 20)
      ; command
          ~description:"Move block up by 20 pixels"
          ~keys:[ key ~shift:() KeyK; key ~shift:() ArrowUp ]
          ~cond:is_not_text_input
          (fun _ev -> add_y (-20))
      ; command
          ~description:"Move block right by 20 pixels"
          ~keys:[ key ~shift:() KeyL; key ~shift:() ArrowRight ]
          ~cond:is_not_text_input
          (fun _ev -> add_x 20)
        (* Open and close help text *)
      ; command
          ~description:"Show keyboard shortcut help"
          ~keys:[ key ~shift:() Slash ]
          ~cond:is_not_text_input
          (fun _ev -> set_show_help true)
      ; command
          ~description:"Exit keyboard shortcut help"
          ~keys:[ key Escape ]
          ~cond:is_not_text_input
          (fun _ev -> set_show_help false)
      ]
  in
  let help_view =
    match%sub show_help with
    | true ->
      let%arr handler = handler in
      Vdom.Node.div
        ~attrs:[ Css.help_container ]
        [ Help_text.view
            (Keyboard_event_handler.get_help_text handler)
            Help_text.View_spec.plain
        ]
    | false -> Bonsai.return Vdom.Node.none
  in
  let%arr x = x
  and y = y
  and handler = handler
  and help_view = help_view in
  let view =
    Vdom.Node.div
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.(Css.block @ style Css_gen.(left (`Px x) @> top (`Px y))) ]
          [ Vdom.Node.div [ Vdom.Node.text "Press ? to see help for keyboard shortcuts" ]
          ]
      ; Vdom.Node.div
          [ Vdom.Node.text
              "Vdom_keyboard supports enabling keybindings when a condition is met. For \
               instance, if a text input is not focused. Try focusing the text input and \
               observe that the keybindings for moving the block around are sent to the \
               text input instead."
          ; Vdom.Node.input ()
          ]
      ; help_view
      ]
  in
  with_keyboard_handler view handler
;;

let () = Bonsai_web.Start.start component
