open! Core
open! Import
open Gen_js_api

type t =
  ([ `y1
   | `y2
   ]
  [@js.enum])

val t_to_js : t -> Ojs.t
