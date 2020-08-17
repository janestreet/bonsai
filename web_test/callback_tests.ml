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

(* Asynchronous callback cant not be tested because you can't write Js_of_ocaml
   async tests*)
