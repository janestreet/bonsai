open! Core
open! Async_kernel
open! Bonsai_web.Cont

val counter : step:int Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t * int Bonsai.t
val counter_ui : Bonsai.graph -> Vdom.Node.t Bonsai.t
