open! Core
open! Bonsai_web

val hello_world : Vdom.Node.t Computation.t
val hello_user : string Value.t -> Vdom.Node.t Computation.t
val hello_textbox : Vdom.Node.t Computation.t
