open! Core
open! Import
open Gen_js_api

type t = Css_gen.Color.t [@@deriving sexp]

val t_to_js : t -> Ojs.t
