open! Core
open! Bonsai_web.Cont

val hello_world : Bonsai.graph -> Vdom.Node.t Bonsai.t
val hello_user : string Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t
val hello_textbox : Bonsai.graph -> Vdom.Node.t Bonsai.t
