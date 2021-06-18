open! Core
open! Bonsai_web
open Bonsai.Let_syntax

(* [HELLO_WORLD_COMPONENT BEGIN] *)
let hello_world : Vdom.Node.t Computation.t =
  Bonsai.const (Vdom.Node.span [ Vdom.Node.text "hello world" ])
;;

(* [HELLO_WORLD_COMPONENT END] *)

(* [HELLO_USER_COMPONENT BEGIN] *)
let hello_user (name : string Value.t) : Vdom.Node.t Computation.t =
  return
  @@ let%map name = name in
  Vdom.Node.span [ Vdom.Node.textf "hello %s" name ]
;;

(* [HELLO_USER_COMPONENT END] *)

(* [HELLO_TEXT_BOX_COMPONENT BEGIN] *)
let hello_textbox : Vdom.Node.t Computation.t =
  let%sub state, set = Bonsai.state [%here] (module String) ~default_model:"" in
  let%sub message = hello_user state in
  return
  @@ let%map message = message
  and set = set in
  Vdom.Node.div
    [ Vdom.Node.input ~attr:(Vdom.Attr.on_input (fun _ text -> set text)) []; message ]
;;

(* [HELLO_TEXT_BOX_COMPONENT BEGIN] *)
