open! Core
open! Bonsai
open! Bonsai_test
module Mst = Bonsai_web_ui_widget.Low_level

let%expect_test _ =
  let computation = Mst.component () in
  let handle =
    Handle.create
      (module struct
        type t = string ref Mst.t
        type incoming = Nothing.t

        let view _ = ""
        let incoming _ = Nothing.unreachable_code
      end)
      computation
  in
  let { Mst.unsafe_init; unsafe_destroy; modify; read } = Handle.last_result handle in
  let print () =
    Ui_effect.Expert.handle
      (let%map.Ui_effect strings = read Ref.( ! ) in
       print_s [%sexp (strings : string list)]);
    Handle.recompute_view handle
  in
  let ref_a = ref "hello" in
  let id_a = unsafe_init ref_a in
  print ();
  [%expect {| (hello) |}];
  ref_a := "world";
  print ();
  [%expect {| (world) |}];
  let ref_b = ref "foo" in
  let id_b = unsafe_init ref_b in
  print ();
  [%expect {| (world foo) |}];
  ref_b := "bar";
  print ();
  [%expect {| (world bar) |}];
  Ui_effect.Expert.handle (modify (fun s -> Ref.replace s String.uppercase));
  print ();
  [%expect {| (WORLD BAR) |}];
  unsafe_destroy id_b;
  print ();
  [%expect {| (WORLD) |}];
  unsafe_destroy id_a;
  print ();
  [%expect {| () |}]
;;
