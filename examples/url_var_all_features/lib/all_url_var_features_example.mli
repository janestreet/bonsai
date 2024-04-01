open! Core
open! Bonsai_web.Cont
module Url_var = Bonsai_web_ui_url_var

module T : sig
  type t [@@deriving sexp, equal]
end

val fallback : Exn.t -> Bonsai_web_ui_url_var.Components.t -> T.t
val versioned_parser : T.t Bonsai_web_ui_url_var.Typed.Versioned_parser.t

val component
  :  url_var:T.t Bonsai_web_ui_url_var.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
