open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax

(* [HELLO_WORLD_COMPONENT BEGIN] *)
let hello_world : Vdom.Node.t Bonsai.Computation.t =
  Bonsai.const (Vdom.Node.span [] [ Vdom.Node.text "hello world" ])
;;

(* [HELLO_WORLD_COMPONENT END] *)

(* [HELLO_USER_COMPONENT BEGIN] *)
let hello_user (name : string Bonsai.Value.t) : Vdom.Node.t Bonsai.Computation.t =
  return
  @@ let%map name = name in
  Vdom.Node.span [] [ Vdom.Node.textf "hello %s" name ]
;;

(* [HELLO_USER_COMPONENT END] *)


(* [HELLO_TEXT_BOX_COMPONENT BEGIN] *)
let hello_textbox : Vdom.Node.t Bonsai.Computation.t =
  let%sub state_and_set = Bonsai.state [%here] (module String) ~default_model:"" in
  let%pattern_bind state, set = state_and_set in
  let%sub message = hello_user state in
  return
  @@ let%map message = message
  and set = set in
  Vdom.Node.div
    []
    [ Vdom.Node.input [ Vdom.Attr.on_input (fun _ text -> set text) ] []; message ]
;;

(* [HELLO_TEXT_BOX_COMPONENT BEGIN] *)
