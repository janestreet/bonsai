open! Core
open! Bonsai_web

let build_result send_message (textbox_content, set_textbox_content) =
  let submit_and_then_clear =
    Vdom.Effect.Many [ send_message textbox_content; set_textbox_content "" ]
  in
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
      if is_key_ret key then submit_and_then_clear else Vdom.Effect.Ignore)
  in
  let on_input = Vdom.Attr.on_input (fun _ -> set_textbox_content) in
  let value = Vdom.Attr.string_property "value" textbox_content in
  let text_input =
    Vdom.Node.input
      ~attr:(Vdom.Attr.many [ on_ret; on_input; value ])
      [ Vdom.Node.text "submit" ]
  in
  let submit_button =
    Vdom_input_widgets.Button.simple "send" ~on_click:(fun _ -> submit_and_then_clear)
  in
  Vdom.Node.div ~attr:(Vdom.Attr.id "compose") [ text_input; submit_button ]
;;

let component ~send_message =
  let open Bonsai.Let_syntax in
  let%sub textbox_state =
    Bonsai.state_machine0
      [%here]
      (module String)
      (module String)
      ~default_model:""
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _ new_state -> new_state)
  in
  return (build_result <$> send_message <*> textbox_state)
;;
