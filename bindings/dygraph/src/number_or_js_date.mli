open! Core
open! Import
open Gen_js_api

type t =
  [ `number of float
  | `date of Js_date.t
  ]

val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t
