open! Core_kernel
open! Import
open Bonsai_web_rpgdice_example

let%expect_test "of_string" =
  let test s =
    let result =
      Or_error.try_with (fun () -> Roll_spec.of_string s)
      |> Or_error.map ~f:Roll_spec.to_string_hum
    in
    print_s [%sexp (result : string Or_error.t)]
  in
  test "2d6 + d4";
  [%expect {| (Ok "2d6 + d4") |}];
  test "d20 + 3d6";
  [%expect {| (Ok "d20 + 3d6") |}];
  test "2d";
  [%expect {| (Error (Roll_spec.of_string "Dice_set.Spec > Die.Spec: count_while1")) |}]
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
