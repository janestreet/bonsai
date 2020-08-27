open! Core_kernel
open! Async_kernel
open! Import
open Bonsai.Let_syntax

module type Conv = sig
  type t

  val of_string : string -> t
  val to_string_hum : t -> string
end

let component (type t) (module Conv : Conv with type t = t) ~default_model =
  let%sub text_state = Bonsai.state [%here] (module String) ~default_model in
  return
  @@ let%map text, set_text = text_state in
  let conv = Or_error.try_with (fun () -> Conv.of_string text) in
  let textbox =
    Vdom.Node.input
      ([ Vdom.Attr.type_ "text"
       ; Vdom.Attr.on_input (fun _ -> set_text)
       ; Vdom.Attr.value text
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
