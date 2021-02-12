open! Core_kernel
open! Import
open Product

let make component ~parse ~unparse =
  let%map.Bonsai.Arrow_deprecated ({ value = { value; view }; set } : _ Same.t) =
    component
  in
  let value = parse value in
  let set v = set (unparse v) in
  { value = { With_view.value; view }; set }
;;

let make_via_string (type t) (module M : Stringable with type t = t) component =
  let parse s = Or_error.try_with (fun () -> M.of_string s) in
  let unparse v = M.to_string v in
  make component ~parse ~unparse
;;
