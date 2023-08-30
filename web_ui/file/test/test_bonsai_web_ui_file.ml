open Core
open Bonsai.Let_syntax
open Bonsai_web_ui_file
open Bonsai_test
module Test_data = For_testing.Test_data

(* Note that this currently only tests our simulator, not the actual use of the underlying
   JavaScript APIs. That's because:

   1. nodejs does not support the HTML APIs we need (File and FileReader). Although there
   are npm packages we could use which purport to provide a compatible interface:
   https://www.npmjs.com/package/file-api
   https://www.npmjs.com/package/filereader

   2. Reading files in javascript is asynchronous, which does not play nicely with
   ppx_expect.
*)

module Read_state = struct
  type file_read = File_read.t

  let equal_file_read = phys_equal

  type t =
    | Starting
    | Reading of (file_read[@sexp.opaque])
    | Finished of (string, Read_error.t) Result.t
  [@@deriving equal, sexp]
end

let set_up_read t =
  let computation =
    let%sub result, set_result =
      Bonsai.state
        Starting
        ~sexp_of_model:[%sexp_of: Read_state.t]
        ~equal:[%equal: Read_state.t]
    in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_result = set_result in
           let open Ui_effect.Let_syntax in
           let%bind file_read =
             read
               ~on_progress:(fun progress ->
                 Ui_effect.print_s [%message "progress" ~_:(progress : Progress.t)])
               t
           in
           let%bind () = set_result (Reading file_read) in
           let%bind read_result = File_read.result file_read in
           set_result (Finished (Result.map ~f:Bigstring.to_string read_result)))
        ()
    in
    Bonsai.read result
  in
  let handle =
    Handle.create
      (module struct
        include Read_state

        type incoming = [ `Abort ]

        let view t = Sexp.to_string [%message "Read result" ~_:(t : t)]

        let incoming t `Abort =
          match t with
          | Starting | Finished _ -> Ui_effect.Ignore
          | Reading read -> File_read.abort read
        ;;
      end)
      computation
  in
  (* Cause the activation stuff to fire *)
  Handle.store_view handle;
  handle
;;

let%expect_test "static" =
  let test_data =
    For_testing.Test_data.create_static ~filename:"foo.txt" ~contents:"foo bar baz"
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  let handle = set_up_read t in
  Handle.show handle;
  [%expect {|
    ("Read result"(Finished(Ok"foo bar baz"))) |}];
  (* Closing does nothing for [create_static] *)
  For_testing.Test_data.close test_data;
  Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Handle.show_diff handle;
  [%expect {||}]
;;

let%expect_test "stream" =
  let test_data =
    For_testing.Test_data.create_stream ~filename:"foo.txt" ~total_bytes:14
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  let handle = set_up_read t in
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 0) (total 14)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.feed_exn test_data "foo";
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 3) (total 14)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.feed_exn test_data "bar baz quux";
  (* Note that we are able to write more than [total_bytes]: there is no validation on
     that parameter. *)
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 15) (total 14)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.close test_data;
  Handle.show handle;
  [%expect {|
    ("Read result"(Finished(Ok"foobar baz quux"))) |}];
  (* Closing again does nothing *)
  For_testing.Test_data.close test_data;
  Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Handle.show_diff handle;
  [%expect {||}]
;;

let%expect_test "stream + close_error" =
  let test_data =
    For_testing.Test_data.create_stream ~filename:"foo.txt" ~total_bytes:14
  in
  let t = For_testing.create test_data in
  print_endline (filename t);
  [%expect {| foo.txt |}];
  let handle = set_up_read t in
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 0) (total 14)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.feed_exn test_data "foo";
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 3) (total 14)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.close_error test_data (Error.of_string "foo");
  Handle.show handle;
  [%expect {|
    ("Read result"(Finished(Error(Error foo)))) |}];
  (* Closing again does nothing *)
  For_testing.Test_data.close test_data;
  Handle.show_diff handle;
  [%expect {||}];
  For_testing.Test_data.close_error test_data (Error.of_string "bar");
  Handle.show_diff handle;
  [%expect {||}]
;;

let%expect_test "abort" =
  let data = Test_data.create_stream ~filename:"foo.txt" ~total_bytes:11 in
  let t = For_testing.create data in
  let handle = set_up_read t in
  Handle.show handle;
  [%expect
    {|
    (progress ((loaded 0) (total 11)))
    ("Read result"(Reading <opaque>)) |}];
  For_testing.Test_data.feed_exn data "hello";
  [%expect {| (progress ((loaded 5) (total 11))) |}];
  Handle.do_actions handle [ `Abort ];
  Handle.show handle;
  [%expect {| ("Read result"(Finished(Error Aborted))) |}]
;;

module Test_read_on_change = struct
  let%expect_test "create_single" =
    let t_var = Bonsai.Var.create None in
    let computation = Read_on_change.create_single_opt (Bonsai.Var.value t_var) in
    let handle =
      Handle.create
        (Result_spec.sexp
           (module struct
             type t = (Filename.t * Read_on_change.Status.t) option [@@deriving sexp_of]
           end))
        computation
    in
    let show () =
      Handle.recompute_view_until_stable handle;
      Handle.show handle
    in
    show ();
    [%expect {| () |}];
    Bonsai.Var.set
      t_var
      (Some
         (For_testing.create
            (Test_data.create_static ~filename:"foo.txt" ~contents:"foo bar baz")));
    show ();
    [%expect {| ((foo.txt (Complete (Ok "foo bar baz")))) |}];
    Bonsai.Var.set t_var None;
    show ();
    [%expect {| () |}];
    let stream_data = Test_data.create_stream ~filename:"bar.txt" ~total_bytes:11 in
    Bonsai.Var.set t_var (Some (For_testing.create stream_data));
    show ();
    [%expect {| ((bar.txt (In_progress ((loaded 0) (total 11))))) |}];
    Test_data.feed_exn stream_data "hello ";
    show ();
    [%expect {| ((bar.txt (In_progress ((loaded 6) (total 11))))) |}];
    Test_data.feed_exn stream_data "world";
    show ();
    [%expect {| ((bar.txt (In_progress ((loaded 11) (total 11))))) |}];
    Test_data.close stream_data;
    show ();
    [%expect {| ((bar.txt (Complete (Ok "hello world")))) |}]
  ;;

  let%expect_test "create_multiple" =
    let ts_var = Bonsai.Var.create Filename.Map.empty in
    let computation = Read_on_change.create_multiple (Bonsai.Var.value ts_var) in
    let handle =
      Handle.create
        (Result_spec.sexp
           (module struct
             type t = Read_on_change.Status.t Filename.Map.t [@@deriving sexp_of]
           end))
        computation
    in
    let show () =
      Handle.recompute_view_until_stable handle;
      Handle.show handle
    in
    show ();
    [%expect {| () |}];
    let map files =
      List.map files ~f:(fun file -> filename file, file) |> Filename.Map.of_alist_exn
    in
    let data1 = Test_data.create_stream ~filename:"foo1.txt" ~total_bytes:11 in
    let data2 = Test_data.create_stream ~filename:"foo2.txt" ~total_bytes:11 in
    let data3 = Test_data.create_stream ~filename:"foo3.txt" ~total_bytes:11 in
    let file1 = For_testing.create data1 in
    let file2 = For_testing.create data2 in
    let file3 = For_testing.create data3 in
    Bonsai.Var.set ts_var (map [ file1; file2; file3 ]);
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (In_progress ((loaded 0) (total 11))))
       (foo3.txt (In_progress ((loaded 0) (total 11))))) |}];
    Test_data.feed_exn data2 "hello";
    Test_data.feed_exn data3 "foo bar";
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (In_progress ((loaded 5) (total 11))))
       (foo3.txt (In_progress ((loaded 7) (total 11))))) |}];
    Bonsai.Var.set ts_var (map [ file1; file2 ]);
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (In_progress ((loaded 5) (total 11))))) |}];
    print_s [%sexp (Test_data.read_status data3 : [ `Aborted | `Not_reading | `Reading ])];
    [%expect {| Aborted |}];
    (* Further writes to data3 should not affect us now *)
    Test_data.feed_exn data3 "xyz";
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (In_progress ((loaded 5) (total 11))))) |}];
    let data4 = Test_data.create_stream ~filename:"foo4.txt" ~total_bytes:13 in
    let file4 = For_testing.create data4 in
    Bonsai.Var.set ts_var (map [ file1; file2; file4 ]);
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (In_progress ((loaded 5) (total 11))))
       (foo4.txt (In_progress ((loaded 0) (total 13))))) |}];
    Test_data.feed_exn data4 "13 characters";
    Test_data.close data4;
    Test_data.close_error data2 (Error.of_string "some file error");
    show ();
    [%expect
      {|
      ((foo1.txt (In_progress ((loaded 0) (total 11))))
       (foo2.txt (Complete (Error "some file error")))
       (foo4.txt (Complete (Ok "13 characters")))) |}];
    (* Verify that if a key in the map gets replaced with a new file, we abort the read of
       the old file. *)
    let data1_new =
      Test_data.create_static ~filename:"foo1.txt" ~contents:"some static file"
    in
    let file1_new = For_testing.create data1_new in
    Bonsai.Var.set ts_var (map [ file1_new; file2; file4 ]);
    show ();
    [%expect
      {|
      ((foo1.txt (Complete (Ok "some static file")))
       (foo2.txt (Complete (Error "some file error")))
       (foo4.txt (Complete (Ok "13 characters")))) |}];
    print_s [%sexp (Test_data.read_status data1 : [ `Aborted | `Not_reading | `Reading ])];
    [%expect {| Aborted |}];
    (* Further changes to [data1] should not have any effect on us now *)
    Test_data.feed_exn data1 "hello world";
    Test_data.close data1;
    show ();
    [%expect
      {|
      ((foo1.txt (Complete (Ok "some static file")))
       (foo2.txt (Complete (Error "some file error")))
       (foo4.txt (Complete (Ok "13 characters")))) |}]
  ;;
end
