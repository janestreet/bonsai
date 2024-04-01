open! Core
open! Async_kernel
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

(* $MDX part-begin=clickies *)
let clickies : Vdom.Node.t =
  (* This won't run until scheduled...
     But it will run every time it is scheduled! *)
  let greet_effect = Effect.alert "hello there!" in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun (_evt : mouse_event) -> greet_effect) ]
        [ Vdom.Node.text "click me!" ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun (_evt : mouse_event) -> greet_effect) ]
        [ Vdom.Node.text "or me!" ]
    ]
;;

(* $MDX part-end *)
let () = Util.run_vdom clickies ~id:"clickies"

(* $MDX part-begin=bind_chain *)
let chain_some_effects
  (a : int Effect.t)
  (b : int -> bool Effect.t)
  (c : unit Effect.t)
  (d : unit Effect.t)
  : unit Effect.t
  =
  let%bind.Effect a_return = a in
  (* Sometimes we don't care about the effect's return value;
     we just want to execute it. *)
  let%bind.Effect (_ : bool) = b a_return in
  let%bind.Effect () = c in
  d
;;

(* $MDX part-end *)

let () = ignore chain_some_effects
