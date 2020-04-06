open! Core_kernel
open Bonsai_web
module Model : T

val single_counter : (unit, Vdom.Node.t) Bonsai.t
val application : (unit, Vdom.Node.t) Bonsai.t
