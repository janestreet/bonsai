open! Core
open! Bonsai_web_test
open! Bonsai_web
open Bonsai_time_example

(* $MDX part-begin=test-clock-component *)
let%expect_test _ =
  let clock = Incr.Clock.create ~start:Time_ns.epoch () in
  let handle = Handle.create ~clock (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:00.000000000Z |}];
  Incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 2.0);
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:02.000000000Z |}]
;;

(* $MDX part-end *)
