open! Core
open! Async_kernel
open! Bonsai_web.Cont
open! Bonsai_web_test

let double_number_app = Rpc_examples.double_number_app
let double_implementation = Rpc_examples.double_implementation

(* $MDX part-begin=attempt-1 *)
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) double_number_app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=open-bonsai-web-test-async *)
open Async_kernel
open Bonsai_web_test_async
(* $MDX part-end *)

let%expect_test "Allowing the async effect of the previous test to run." =
  let handle = Handle.create (Result_spec.vdom Fn.id) double_number_app in
  let%bind () = Handle.flush_async_and_bonsai handle in
  [%expect
    {|
    (Failure "BUG: no bonsai-rpc handler installed")
    ------ between bonsai frame ------
    |}];
  return ()
;;

(* $MDX part-begin=attempt-2 *)
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) double_number_app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  let%bind () = Handle.flush_async_and_bonsai handle in
  [%expect {| (Failure "BUG: no bonsai-rpc handler installed") |}];
  return ()
;;

(* $MDX part-end *)

(* $MDX part-begin=attempt-3 *)
let%expect_test "Clicking the button should double the number" =
  let handle =
    Handle.create
      ~rpc_implementations:[ double_implementation ]
      (Result_spec.vdom Fn.id)
      double_number_app
  in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  let%bind () = Handle.flush_async_and_bonsai handle in
  Handle.show handle;
  [%expect
    {|
    ------ between bonsai frame ------
    <div>
      <div> The number is: 2 </div>
      <button onclick> Double the number </button>
    </div>
    |}];
  return ()
;;
(* $MDX part-end *)
