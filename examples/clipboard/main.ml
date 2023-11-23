open! Core
open! Bonsai_web
open Js_of_ocaml
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

let component =
  let%sub form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let%sub copy_button =
    let%arr form = form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            match%bind.Effect
              Js_clipboard.Asynchronous.copy_text
                (Js.string (Form.value_or_default form ~default:""))
            with
            | Ok () -> Effect.Ignore
            | Error err ->
              Effect.print_s [%message "Failed to copy to clipboard." (err : Error.t)])
        ]
      [ Vdom.Node.text "copy to clipboard" ]
  in
  (* NOTE: This button copies from clipboard in a deprecated and non-recommended way. Use
     the above button implementation as a reference if you'd like clipboard copying
     behavior in your app. *)
  let%sub legacy_copy_button =
    let%arr form = form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            Ui_effect.Expert.handle
              (Effect.ignore_m
                 (Js_clipboard.Asynchronous.copy_text
                    (Js.string (Form.value_or_default form ~default:""))));
            Effect.Ignore)
        ]
      [ Vdom.Node.text "copy to clipboard (legacy behaviour)" ]
  in
  let%sub paste_button =
    let%arr form = form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            let%bind.Effect st = Js_clipboard.Asynchronous.read_text in
            match st with
            | Ok st -> Form.set form (Js.to_string st)
            | Error error ->
              Effect.print_s
                [%message "Error reading from clipboard" ~_:(error : Error.t)])
        ]
      [ Vdom.Node.text "paste from clipboard" ]
  in
  let%arr form = form
  and copy_button = copy_button
  and legacy_copy_button = legacy_copy_button
  and paste_button = paste_button in
  Vdom.Node.div [ Form.view_as_vdom form; copy_button; legacy_copy_button; paste_button ]
;;

let () = Start.start component
