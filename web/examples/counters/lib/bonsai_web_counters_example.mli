open! Core_kernel
open Bonsai_web
module Model : T

val application_component : (unit, Model.t, Vdom.Node.t) Bonsai.t
val initial_model : Model.t
