open! Core
open! Js_of_ocaml

module Arrow_deprecated = struct
  module Effect = Effect
  module Vdom = Import.Vdom
  module Start = Start.Arrow_deprecated
  module Bonsai = Bonsai.Arrow_deprecated
end

module Start = Start.Proc
module Bonsai = Import.Bonsai
module Incr = Import.Incr
module Vdom = Import.Vdom
module To_incr_dom = To_incr_dom
module Effect = Effect
module Persistent_var = Persistent_var

(* new *)
module Computation = Bonsai.Computation
module Value = Bonsai.Value

(* [am_running_how] provides information on how the code is currently being run:
   - [`Node_test] means that the code is being run using node as part of an expect_test
   - [`Node_benchmark] means that the code is being run using node as part of a benchmark
   - [`Node] means that the code is being run using node, but not as part of an
     expect_test or a benchmark
   - [`Browser_benchmark] means that the code is being run in the browser as part of a
     benchmark
   - [`Browser] means that the code is being run in a browser but not as part of a
     benchmark
*)

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

(* [am_within_disabled_fieldset] traverses up the DOM to see whether an event occurred
   within a fieldset element with the disabled attribute. As this function requires DOM
   interaction, it will return [false] if the code is not running in the browser.

   Note: because this function bubbles up from the target of the event, it's possible
   that the event occurs within a disabled fieldset, but the form element which performs
   this check is not within a disabled fieldset (or vice versa).
   For example, mousemove events will originate from the element under the mouse, so if
   the mouse is over a different disabled form, [am_within_disabled_fieldset] will be
   [true], even if the component which performs this check is not.
*)
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
