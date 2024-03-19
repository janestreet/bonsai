open! Core
open Bonsai_web
open Bonsai_web_test
module Gauge = Bonsai_web_ui_gauge
open Bonsai.Let_syntax

let percent_to_color (p : Percent.t) : Css_gen.Color.t =
  `RGBA (Css_gen.Color.RGBA.create ~r:0 ~g:0 ~b:0 ~a:p ())
;;

let%expect_test _ =
  let percent = Bonsai.Var.create Percent.zero in
  let component =
    let%arr percent = Bonsai.Var.value percent in
    Gauge.create ~percent_to_color ~radius:20. percent
  in
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect
    {|
    <div class="wrapper_hash_replaced_in_test">
      <svg height="40" width="40">
        <circle cx="20"
                cy="20"
                fill="transparent"
                r="18"
                stroke="#E5E7EB"
                stroke-width="4"
                stroke-dasharray="84.82300164692441,113.09733552923255"
                transform="rotate(135, 20, 20)"
                stroke-linecap="round"> </circle>
        <circle cx="20"
                cy="20"
                fill="transparent"
                r="18"
                stroke="rgba(0,0,0,0.00)"
                stroke-width="4"
                stroke-dasharray="84.82300164692441,113.09733552923255"
                transform="rotate(135, 20, 20)"
                stroke-linecap="round"
                stroke-dashoffset="84.82300164692441"> </circle>
      </svg>
    </div>
    |}];
  Bonsai.Var.set percent (Percent.of_mult 0.5);
  Handle.show_diff handle;
  [%expect
    {|
      <div class="wrapper_hash_replaced_in_test">
        <svg height="40" width="40">
          <circle cx="20"
                  cy="20"
                  fill="transparent"
                  r="18"
                  stroke="#E5E7EB"
                  stroke-width="4"
                  stroke-dasharray="84.82300164692441,113.09733552923255"
                  transform="rotate(135, 20, 20)"
                  stroke-linecap="round"> </circle>
          <circle cx="20"
                  cy="20"
                  fill="transparent"
                  r="18"
    -|            stroke="rgba(0,0,0,0.00)"
    +|            stroke="rgba(0,0,0,0.50)"
                  stroke-width="4"
                  stroke-dasharray="84.82300164692441,113.09733552923255"
                  transform="rotate(135, 20, 20)"
                  stroke-linecap="round"
    -|            stroke-dashoffset="84.82300164692441"> </circle>
    +|            stroke-dashoffset="42.411500823462205"> </circle>
        </svg>
      </div>
    |}];
  Bonsai.Var.set percent Percent.one_hundred_percent;
  Handle.show_diff handle;
  [%expect
    {|
      <div class="wrapper_hash_replaced_in_test">
        <svg height="40" width="40">
          <circle cx="20"
                  cy="20"
                  fill="transparent"
                  r="18"
                  stroke="#E5E7EB"
                  stroke-width="4"
                  stroke-dasharray="84.82300164692441,113.09733552923255"
                  transform="rotate(135, 20, 20)"
                  stroke-linecap="round"> </circle>
          <circle cx="20"
                  cy="20"
                  fill="transparent"
                  r="18"
    -|            stroke="rgba(0,0,0,0.50)"
    +|            stroke="rgba(0,0,0,1.00)"
                  stroke-width="4"
                  stroke-dasharray="84.82300164692441,113.09733552923255"
                  transform="rotate(135, 20, 20)"
                  stroke-linecap="round"
    -|            stroke-dashoffset="42.411500823462205"> </circle>
    +|            stroke-dashoffset="0"> </circle>
        </svg>
      </div>
    |}]
;;
