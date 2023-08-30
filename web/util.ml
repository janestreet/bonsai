open! Core
open! Js_of_ocaml

let am_running_how
  : [ `Node_test | `Node_benchmark | `Node | `Browser_benchmark | `Browser ]
  =
  let is_in_browser = Js.Optdef.test (Obj.magic Dom_html.document : _ Js.Optdef.t) in
  let is_benchmark =
    match Sys.getenv "BENCHMARKS_RUNNER" with
    | Some "TRUE" -> true
    | _ -> false
  in
  match is_in_browser, is_benchmark, Core.am_running_test with
  | true, true, _ -> `Browser_benchmark
  | true, false, true -> Core.raise_s [%message "cannot run tests in a browser"]
  | true, false, false -> `Browser
  | false, true, _ -> `Node_benchmark
  | false, false, true -> `Node_test
  | false, false, false -> `Node
;;

let am_within_disabled_fieldset (event : #Dom_html.event Js.t) =
  match am_running_how with
  | `Node_test | `Node_benchmark | `Node -> false
  | `Browser | `Browser_benchmark ->
    let (event : < composedPath : 'a Js.js_array Js.t Js.meth ; Dom_html.event > Js.t) =
      Js.Unsafe.coerce event
    in
    Js.to_array event##composedPath
    |> Array.exists ~f:(fun element ->
         let tag_name = Js.Optdef.to_option element##.tagName in
         let disabled = Js.Optdef.to_option element##.disabled in
         match Option.both tag_name disabled with
         | None -> false
         | Some (tag_name, disabled) ->
           String.equal (Js.to_string tag_name) "FIELDSET" && Js.to_bool disabled)
;;
