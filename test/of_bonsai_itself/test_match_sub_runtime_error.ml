open! Core
open! Import
open Bonsai.Let_syntax
open Bonsai_test

let%expect_test "match%sub defers exceptions until runtime" =
  let var = Bonsai.Var.create true in
  let component =
    match%sub Bonsai.Var.value var with
    | true -> Bonsai.const "yay!"
    | false -> assert false
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  [%expect {| yay! |}];
  Bonsai.Var.set var false;
  Expect_test_helpers_core.require_does_raise ~hide_positions:true [%here] (fun () ->
    Handle.show handle);
  [%expect {| "Assert_failure test_match_sub_runtime_error.ml:LINE:COL" |}]
;;
