open! Core
open! Import
open Gen_js_api

type t = Css_gen.Color.t [@@deriving sexp]

let t_to_js t = Ojs.string_to_js (Css_gen.Color.to_string_css t)
