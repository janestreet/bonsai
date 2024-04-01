open! Core
open! Bonsai_web.Cont

(* $MDX part-begin=hello_world *)
let hello_world : Vdom.Node.t = Vdom.Node.text "hello world!"

(* $MDX part-end *)

let () = Util.run_vdom hello_world ~id:"hello_world"

(* $MDX part-begin=bulleted_list *)
let bulleted_list : Vdom.Node.t =
  let open Vdom.Node in
  div
    [ h3 [ text "Norwegian Pancakes" ]
    ; ul
        [ li [ text "3 eggs" ]
        ; li [ text "2 cups of milk" ]
        ; li [ text "1 cup of flour" ]
        ]
    ]
;;

(* $MDX part-end *)

let () = Util.run_vdom bulleted_list ~id:"bulleted_list"

(* $MDX part-begin=input_placeholder *)
let input_placeholder : Vdom.Node.t =
  Vdom.Node.input ~attrs:[ Vdom.Attr.placeholder "placeholder text here" ] ()
;;

(* $MDX part-end *)
let () = Util.run_vdom input_placeholder ~id:"input_placeholder"

(* $MDX part-begin=css *)
let css : Vdom.Node.t =
  Vdom.Node.span ~attrs:[ [%css {|color: red;|}] ] [ Vdom.Node.text "this text is red" ]
;;

(* $MDX part-end *)
let () = Util.run_vdom css ~id:"css"

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

(* $MDX part-begin=clicky_button *)
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun (_evt : mouse_event) ->
          (* Alerts are generally bad UI; there's an `Effect.print_s` for logging *)
          Effect.alert "hello there!")
      ]
    [ Vdom.Node.text "click me!" ]
;;

(* $MDX part-end *)

let () = Util.run_vdom clicky ~id:"clicky_button"
