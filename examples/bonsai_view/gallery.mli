open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Codemirror = Bonsai_web_ui_codemirror

module type Demo = sig
  val name : string
  val description : string
  val view : (Vdom.Node.t * string) Computation.t
  val selector : string option
  val filter_attrs : (string -> string -> bool) option
end

val make_demo : (module Demo) -> Vdom.Node.t Computation.t

val make_sections
  :  theme_picker:Vdom.Node.t Value.t
  -> (string * string * Vdom.Node.t Computation.t list) list
  -> Vdom.Node.t Computation.t
