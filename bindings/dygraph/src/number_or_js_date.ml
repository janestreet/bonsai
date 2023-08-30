open! Core
open! Import
open Gen_js_api

type t =
  [ `number of float
  | `date of Js_date.t
  ]

let t_to_js = function
  | `number f -> Ojs.float_to_js f
  | `date d -> Js_date.t_to_js d
;;

let t_of_js ojs =
  if String.equal "number" (Ojs.type_of ojs)
  then `number (Ojs.float_of_js ojs)
  else `date (Js_date.t_of_js ojs)
;;
