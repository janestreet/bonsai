open! Core
open! Bonsai_web
open! Bonsai_web_test
open Async_rpc_kernel

let double_rpc =
  Rpc.Rpc.create
    ~name:"double"
    ~version:0
    ~bin_query:[%bin_type_class: int]
    ~bin_response:[%bin_type_class: int]
    ~include_in_error_count:Only_on_exn
;;

let double_implementation =
  Rpc.Rpc.implement' double_rpc (fun _connection_state query -> Int.max 1 (query * 2))
;;

let where_to_connect : Rpc_effect.Where_to_connect.t = Self

let app =
  let open Bonsai.Let_syntax in
  let%sub dispatch_double_rpc = Rpc_effect.Rpc.dispatcher double_rpc ~where_to_connect in
  let%sub number, set_number = Bonsai.state ~equal:[%equal: int] 1 in
  let%arr dispatch_double_rpc = dispatch_double_rpc
  and number = number
  and set_number = set_number in
  Vdom.Node.div
    [ Vdom.Node.div [ Vdom.Node.text [%string "The number is: %{number#Int}"] ]
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              match%bind.Effect dispatch_double_rpc number with
              | Ok doubled_number -> set_number doubled_number
              | Error error -> Effect.of_sync_fun print_s [%sexp (error : Error.t)])
          ]
        [ Vdom.Node.text "Double the number" ]
    ]
;;

(* $MDX part-begin=attempt-1 *)
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=open-async-js-test *)
open Async_kernel
open Async_js_test
(* $MDX part-end *)

let%expect_test "Allowing the async effect of the previous test to run." =
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  [%expect {| (Failure "BUG: no bonsai-rpc handler installed") |}];
  return ()
;;

(* $MDX part-begin=attempt-2 *)
let%expect_test "Clicking the button should double the number" =
  let handle = Handle.create (Result_spec.vdom Fn.id) app in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
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
      app
  in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 1 </div>
      <button onclick> Double the number </button>
    </div> |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"button";
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> The number is: 2 </div>
      <button onclick> Double the number </button>
    </div> |}];
  return ()
;;
(* $MDX part-end *)
