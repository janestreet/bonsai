open! Core
open! Bonsai_web

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
let alert s = Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string s)

(* $MDX part-begin=input_placeholder *)
let input_placeholder : Vdom.Node.t =
  Vdom.Node.input ~attr:(Vdom.Attr.placeholder "placeholder text here") []
;;

(* $MDX part-end *)
let () = Util.run_vdom input_placeholder ~id:"input_placeholder"

(* $MDX part-begin=css_gen *)
let css_gen : Vdom.Node.t =
  Vdom.Node.span
    ~attr:(Vdom.Attr.style (Css_gen.color (`Name "red")))
    [ Vdom.Node.text "this text is red" ]
;;

(* $MDX part-end *)
let () = Util.run_vdom css_gen ~id:"css_gen"

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

(* Running side-effects directly inside an event-handler is not ideal, but I
   didn't want to take a dependency on "Effect"s before introducing Events.

   In real code, prefer passing in an "inject_alert" function that is backed by
   [Effect.of_sync_fun].  That would make this component testable ([alert] is not
   supported in nodejs) *)
(* $MDX part-begin=clicky_button *)
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attr:
      (Vdom.Attr.on_click (fun (_evt : mouse_event) ->
         alert "hello there!";
         Ui_effect.Ignore))
    [ Vdom.Node.text "click me!" ]
;;

(* $MDX part-end *)

let () = Util.run_vdom clicky ~id:"clicky_button"
