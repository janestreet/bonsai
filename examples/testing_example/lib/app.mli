open! Core_kernel
open! Bonsai_web

val hello_world : Vdom.Node.t Bonsai.Computation.t
val hello_user : string Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t
val hello_textbox : Vdom.Node.t Bonsai.Computation.t
