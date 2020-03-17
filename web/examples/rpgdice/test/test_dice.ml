open! Core_kernel
open! Import
open Bonsai_web_rpgdice_example

let%expect_test "of_string" =
  let test s = print_endline (Roll_spec.to_string_hum (Roll_spec.of_string s)) in
  test "2d6 + d4";
  [%expect {| 2d6 + d4 |}];
  test "d20 + 3d6";
  [%expect {| d20 + 3d6 |}]
;;

let%expect_test "roll" =
  let test s =
    print_s ([%sexp_of: Roll_result.t] (Roll_spec.roll (Roll_spec.of_string s)))
  in
  test "2d6 + d4";
  [%expect
    {|
    ((dice (
       ((num_faces 6) (result 1))
       ((num_faces 6) (result 1))
       ((num_faces 4) (result 1))))
     (const 0)) |}];
  test "d20 + 3d6";
  [%expect
    {|
    ((dice (
       ((num_faces 20) (result 11))
       ((num_faces 6)  (result 1))
       ((num_faces 6)  (result 4))
       ((num_faces 6)  (result 4))))
     (const 0)) |}]
;;
