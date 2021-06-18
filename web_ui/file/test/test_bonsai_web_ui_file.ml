open Core
open Bonsai.Let_syntax
open Bonsai_web_ui_file

(* Note that this currently only tests our simulator, not the actual use of the underlying
   JavaScript APIs. That's because:

   1. nodejs does not support the HTML APIs we need (File and FileReader). Although there
   are npm packages we could use which purport to provide a compatible interface:
   https://www.npmjs.com/package/file-api
   https://www.npmjs.com/package/filereader

   2. Reading files in javascript is asynchronous, which does not play nicely with
   ppx_expect.
*)

let print_contents t =
  contents t
  |> Bonsai.Effect.inject ~on_response:(fun contents ->
    print_s [%message "print_contents completed" (contents : string Or_error.t)];
    Ui_event.Ignore)
  |> Ui_event.Expert.handle
;;

let set_up_read t =
  let read = read t in
  let computation =
    let module Result = struct
      type t = (string, File_read.error) Result.t [@@deriving compare, sexp_of]

      let t_of_sexp = opaque_of_sexp
      let equal = [%compare.equal: t]
    end
    in
    let%sub result, set_result = Bonsai.state_opt [%here] (module Result) in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_result = set_result in
           File_read.result read
           |> Bonsai.Effect.inject ~on_response:(fun resp -> set_result (Some resp)))
        ()
    in
    Bonsai.read
      (let%map state = File_read.state read
       and result = result in
       sprintf
         !"State: %{sexp:File_read.State.t}\nResult: %{sexp:Result.t option}"
         state
         result)
  in
  let handle =
    Bonsai_test.Handle.create (Bonsai_test.Result_spec.string (module String)) computation
  in
  (* Cause the activation stuff to fire *)
  Bonsai_test.Handle.store_view handle;
  handle
;;

let%expect_test "static" =
  let test_data =
    For_testing.Test_data.create_static ~filename:"foo.txt" ~contents:"foo bar baz"
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  print_contents t;
  [%expect {| ("print_contents completed" (contents (Ok "foo bar baz"))) |}];
  let handle = set_up_read t in
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Contents "foo bar baz")
    Result: ((Ok "foo bar baz")) |}];
  (* Closing does nothing for [create_static] *)
  For_testing.Test_data.close test_data;
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}]
;;

let%expect_test "stream" =
  let test_data =
    For_testing.Test_data.create_stream ~filename:"foo.txt" ~total_bytes:14
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  print_contents t;
  let handle = set_up_read t in
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Loading (((loaded 0) (total 14))))
    Result: () |}];
  For_testing.Test_data.feed_exn test_data "foo";
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Loading (((loaded 3) (total 14))))
    Result: () |}];
  For_testing.Test_data.feed_exn test_data "bar baz quux";
  (* Note that we are able to write more than [total_bytes]: there is no validation on
     that parameter. *)
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Loading (((loaded 15) (total 14))))
    Result: () |}];
  For_testing.Test_data.close test_data;
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    ("print_contents completed" (contents (Ok "foobar baz quux")))
    State: (Contents "foobar baz quux")
    Result: ((Ok "foobar baz quux")) |}];
  (* Closing again does nothing *)
  For_testing.Test_data.close test_data;
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}]
;;

let%expect_test "stream + close_error" =
  let test_data =
    For_testing.Test_data.create_stream ~filename:"foo.txt" ~total_bytes:14
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  print_contents t;
  let handle = set_up_read t in
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Loading (((loaded 0) (total 14))))
    Result: () |}];
  For_testing.Test_data.feed_exn test_data "foo";
  Bonsai_test.Handle.show handle;
  [%expect {|
    State: (Loading (((loaded 3) (total 14))))
    Result: () |}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    ("print_contents completed" (contents (Error foo)))
    State: (Error foo)
    Result: ((Error (Error foo))) |}];
  (* Closing again does nothing *)
  For_testing.Test_data.close test_data;
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "bar");
  Bonsai_test.Handle.show_diff handle;
  [%expect {||}]
;;
