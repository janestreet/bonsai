open! Core
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=hello-world-component *)
let hello_world : Vdom.Node.t Computation.t =
  Bonsai.const (Vdom.Node.span [ Vdom.Node.text "hello world" ])
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-user-component *)
let hello_user (name : string Value.t) : Vdom.Node.t Computation.t =
  let%arr name = name in
  Vdom.Node.span [ Vdom.Node.textf "hello %s" name ]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-text-box-component *)
let hello_textbox : Vdom.Node.t Computation.t =
  let%sub state, set = Bonsai.state (module String) ~default_model:"" in
  let%sub message = hello_user state in
  let%arr message = message
  and set = set in
  Vdom.Node.div
    [ Vdom.Node.input ~attrs:[ Vdom.Attr.on_input (fun _ text -> set text) ] (); message ]
;;
(* $MDX part-end *)
