open! Core
open! Import
open Bonsai_test
open Bonsai.For_open

let handle () =
  let var = Bonsai.Var.create 0 in
  let handle =
    Handle.create
      (module struct
        type t = int
        type incoming = unit

        let view i =
          print_endline "computing the view!";
          Int.to_string i
        ;;

        let incoming _i () = Effect.Ignore
      end)
      (Bonsai.read (Bonsai.Var.value var))
  in
  handle, var
;;

let%expect_test "Handle.show forces the view" =
  let handle, var = handle () in
  Handle.show handle;
  [%expect {|
    computing the view!
    0
    |}];
  Bonsai.Var.set var 1;
  Handle.show handle;
  [%expect {|
    computing the view!
    1
    |}]
;;

let%expect_test "Handle.recompute_view should _not_ force the view" =
  let handle, var = handle () in
  Handle.recompute_view handle;
  [%expect {| |}];
  Bonsai.Var.set var 1;
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.show handle;
  [%expect {|
    computing the view!
    1
    |}]
;;
