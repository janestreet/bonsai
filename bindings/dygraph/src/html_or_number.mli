open! Core
open! Import
open Gen_js_api

type t =
  [ `html of Raw_html.t
  | `number of float
  ]
[@@deriving compare, equal, sexp]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
