open! Core
open! Async_kernel
open Bonsai_web
open Bonsai_web_test
open Async_rpc_kernel
open Async_js_test
open Expect_test_helpers_core

let rpc_a =
  Rpc.Rpc.create
    ~name:"a"
    ~version:0
    ~bin_query:bin_unit
    ~bin_response:bin_unit
    ~include_in_error_count:Only_on_exn
;;

let computation iterations =
  let open Bonsai.Let_syntax in
  let%sub count, increment =
    Bonsai.state_machine0
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model () -> model + 1)
      ()
  in
  let%sub dispatcher = Rpc_effect.Rpc.dispatcher rpc_a ~where_to_connect:Self in
  let%sub callback =
    let%arr dispatcher = dispatcher
    and increment = increment in
    fun count ->
      if count < iterations
      then (
        let%bind.Effect () = Effect.print_s [%message "iteration" (count : int)] in
        let%bind.Effect (_ : unit Or_error.t) = dispatcher () in
        increment ())
      else Effect.print_s [%message "finished loop"]
  in
  Bonsai.Edge.on_change ~equal:[%equal: int] count ~callback
;;

let%expect_test _ =
  let rpc_implementations = [ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ] in
  let handle =
    Handle.create ~rpc_implementations (Result_spec.string (module Unit)) (computation 3)
  in
  Handle.show handle;
  [%expect {|
    ()
    (iteration (count 0)) |}];
  Handle.show handle;
  [%expect {| () |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.show handle;
  [%expect {|
    ()
    (iteration (count 1)) |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.show handle;
  [%expect {|
    ()
    (iteration (count 2)) |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.show handle;
  [%expect {|
    ()
    "finished loop" |}];
  let handle =
    Handle.create ~rpc_implementations (Result_spec.string (module Unit)) (computation 3)
  in
  let%bind () = Handle.flush_async_and_bonsai handle in
  [%expect
    {|
    (iteration (count 0))
    (iteration (count 1))
    ------ between bonsai frame ------
    (iteration (count 2))
    ------ between bonsai frame ------
    "finished loop"
    ------ between bonsai frame ------ |}];
  return ()
;;

let%expect_test _ =
  let rpc_implementations = [ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ] in
  let handle =
    Handle.create ~rpc_implementations (Result_spec.string (module Unit)) (computation 10)
  in
  match%map
    Async_kernel.try_with (fun () ->
      Handle.flush_async_and_bonsai ~max_iterations:5 handle)
  with
  | Ok () -> ()
  | Error exn ->
    print_s [%message (exn : exn)];
    [%expect
      {|
    (iteration (count 0))
    (iteration (count 1))
    ------ between bonsai frame ------
    (iteration (count 2))
    ------ between bonsai frame ------
    (iteration (count 3))
    ------ between bonsai frame ------
    (iteration (count 4))
    ------ between bonsai frame ------
    (iteration (count 5))
    (exn (monitor.ml.Error "not stable after 5 iterations")) |}]
;;

let%expect_test _ =
  let rpc_implementations = [ Rpc.Rpc.implement' rpc_a (fun _ query -> query) ] in
  let handle =
    Handle.create ~rpc_implementations (Result_spec.string (module Unit)) (computation 10)
  in
  match%map
    Async_kernel.try_with (fun () ->
      Handle.flush_async_and_bonsai ~max_iterations:5 ~silence_between_frames:true handle)
  with
  | Ok () -> ()
  | Error exn ->
    print_s [%message (exn : exn)];
    [%expect
      {|
    (iteration (count 0))
    (iteration (count 1))
    (iteration (count 2))
    (iteration (count 3))
    (iteration (count 4))
    (iteration (count 5))
    (exn (monitor.ml.Error "not stable after 5 iterations")) |}]
;;
