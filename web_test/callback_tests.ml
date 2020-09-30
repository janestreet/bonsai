open! Core_kernel
open! Async_kernel
open Bonsai_web

let schedule_and_print_response callback =
  Vdom.Event.Expert.handle_non_dom_event_exn
    (Effect.inject_with_userdata
       callback
       ~userdata:"this is userdata"
       ~on_response:(fun r u ->
         print_s [%message (u : string) (r : int)];
         Vdom.Event.Ignore))
;;

let%expect_test "synchronous callback" =
  let increment = unstage @@ Effect.of_sync_fun (fun a -> a + 1) in
  schedule_and_print_response (increment 1);
  [%expect {| ((u "this is userdata") (r 2)) |}]
;;

let%expect_test "bind and return" =
  let open Effect.Let_syntax in
  let increment = unstage @@ Effect.of_sync_fun (fun a -> a + 1) in
  let callback =
    let%bind a = increment 1 in
    let%bind b = increment 2 in
    return (a + b)
  in
  schedule_and_print_response callback;
  [%expect {| ((u "this is userdata") (r 5)) |}]
;;

let%expect_test "handle error" =
  let is_even =
    unstage
    @@ Effect.of_sync_fun (fun a ->
      if a mod 2 = 0 then Ok a else Error (Error.of_string "not even"))
  in
  let callback a =
    Effect.handle_error (is_even a) ~f:(fun error ->
      print_endline (Error.to_string_hum error);
      Vdom.Event.Ignore)
  in
  schedule_and_print_response (callback 1);
  schedule_and_print_response (callback 2);
  [%expect {|
    not even
    ((u "this is userdata") (r 2)) |}]
;;

let%expect_test "of_deferred_fun" =
  (* Note that there is no module that can drive the async_kernel scheduler that is
     compatible with javascript tests. The only way therefore of testing of_deferred_fun
     is to instruct the scheduler ourselves. *)
  let open Async_kernel in
  let state = Int.Table.create () in
  let callback =
    unstage
      (Effect.of_deferred_fun (fun n ->
         Ivar.read (Hashtbl.find_or_add state n ~default:Ivar.create)))
  in
  schedule_and_print_response (callback 1);
  schedule_and_print_response (callback 2);
  schedule_and_print_response (callback 3);
  [%expect {||}];
  print_s [%sexp (Hashtbl.keys state : int list)];
  [%expect {| (1 3 2) |}];
  Ivar.fill (Hashtbl.find_and_remove state 1 |> Option.value_exn) 42;
  (* Nothing yet, because the scheduler has not run a cycle *)
  [%expect {||}];
  (* Tell async_kernel to run a cycle manually to see that the effect is responded to *)
  Async_kernel_scheduler.Private.run_cycles_until_no_jobs_remain ();
  [%expect {| ((u "this is userdata") (r 42)) |}];
  Hashtbl.iteri state ~f:(fun ~key:query ~data:ivar -> Ivar.fill ivar (10 * query));
  Async_kernel_scheduler.Private.run_cycles_until_no_jobs_remain ();
  [%expect {|
    ((u "this is userdata") (r 20))
    ((u "this is userdata") (r 30)) |}]
;;

let%expect_test "svar" =
  let in_flight_query_and_response = ref None in
  let callback =
    unstage
      (Effect.For_testing.of_svar_fun (fun query ->
         let svar = Effect.For_testing.Svar.create () in
         in_flight_query_and_response := Some (query, svar);
         svar))
  in
  schedule_and_print_response (callback 1);
  [%expect {||}];
  let query, response = Option.value_exn !in_flight_query_and_response in
  Effect.For_testing.Svar.fill_if_empty response (query + 1);
  [%expect {| ((u "this is userdata") (r 2)) |}]
;;

let%expect_test "Query_response_tracker" =
  let qrt = Effect.For_testing.Query_response_tracker.create () in
  let callback = unstage (Effect.For_testing.of_query_response_tracker qrt) in
  schedule_and_print_response (callback 1);
  schedule_and_print_response (callback 2);
  schedule_and_print_response (callback 3);
  [%expect {||}];
  Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun i ->
    if i % 2 = 0 then Respond (10 * i) else No_response_yet);
  [%expect {| ((u "this is userdata") (r 20)) |}];
  printf
    !"%{sexp: int list}"
    (Effect.For_testing.Query_response_tracker.queries_pending_response qrt);
  [%expect {| (3 1) |}];
  Effect.For_testing.Query_response_tracker.maybe_respond qrt ~f:(fun i ->
    Respond (20 * i));
  [%expect {|
    ((u "this is userdata") (r 60))
    ((u "this is userdata") (r 20)) |}];
  printf
    !"%{sexp: int list}"
    (Effect.For_testing.Query_response_tracker.queries_pending_response qrt);
  [%expect {| () |}]
;;
