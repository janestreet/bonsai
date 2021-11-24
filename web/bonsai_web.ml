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
  let open Js_of_ocaml in
  let is_in_browser = Js.Optdef.test (Obj.magic Dom_html.document : _ Js.Optdef.t) in
  let is_benchmark =
    match Sys.getenv_opt "BENCHMARKS_RUNNER" with
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
