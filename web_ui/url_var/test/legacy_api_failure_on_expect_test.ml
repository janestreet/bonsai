open! Core
module Url_var = Bonsai_web_ui_url_var

let%expect_test "self-documenting error also occurs on legacy api." =
  (* NOTE: The reason this test needs its own file is so that the history API is not called
     twice as it has a check that gives off a different error message when the history API is called twice.
     Putting the calls in different files removes the shared context. *)
  (match
     Url_var.create_exn
       (module struct
         type t = int [@@deriving sexp, equal]

         let parse_exn _ = 1
         let unparse _ = Url_var.Components.create ()
       end)
       ~fallback:1
   with
   | _ -> failwith "creating should fail in test"
   | exception Failure message -> print_endline message);
  [%expect
    {|
    Error: Bonsai_web_ui_url_var.create_exn is not supported within a nodejs
    environment because it relies on the browser's history API. One way to fix this
    is by having your app receive the url value as a parameter, and passing some
    mock implementation in tests instead of the real implementation provided by this
    library.
    |}]
;;
