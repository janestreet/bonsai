open! Core
open! Import
open Gen_js_api

type t =
  [ `html of Raw_html.t
  | `number of float
  ]
[@@deriving compare, equal, sexp]

let t_to_js = function
  | `html s -> Raw_html.t_to_js s
  | `number f -> Ojs.float_to_js f
;;

let t_of_js ojs =
  if String.equal "number" (Ojs.type_of ojs)
  then `number (Ojs.float_of_js ojs)
  else `html (Raw_html.t_of_js ojs)
;;
