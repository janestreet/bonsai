open! Core
open! Import
open Gen_js_api

type t = Ojs.t

let t_to_js x = x
let t_of_js x = x

let create ?options ?data () =
  (* This is intentionally different than an object with a property "options". *)
  let options =
    let default = Ojs.empty_obj () in
    Option.value_map options ~default ~f:Options.t_to_js
  in
  Option.iter data ~f:(fun data -> Ojs.set_prop_ascii options "file" (Data.t_to_js data));
  options
;;
