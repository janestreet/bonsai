open! Core
open! Bonsai_web_test
open! Bonsai_web.Cont
open Bonsai_time_example

(* $MDX part-begin=test-clock-component *)
let%expect_test _ =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:00.000000000Z |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:02.000000000Z |}]
;;

(* $MDX part-end *)
