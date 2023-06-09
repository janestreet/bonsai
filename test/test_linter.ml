open! Core
open! Import
open Bonsai.Let_syntax
open Bonsai.For_open
module Private = Bonsai.Private

(* Each test sets this ref to the location at which the test began, so that
   test output can be made resilient to adding or remove lines above the test. *)
let test_location_reference_point = ref [%here]
let test_start here = test_location_reference_point := here

let test_lint computation =
  let computation = Private.reveal_computation computation in
  List.iter (Private.Linter.list_warnings computation) ~f:(fun warning ->
    print_endline
      (Private.Linter.Warning.to_string
         (Private.Linter.Warning.relative_to !test_location_reference_point warning)))
;;

let constant_fold computation =
  computation
  |> Private.reveal_computation
  |> Private.Constant_fold.constant_fold
  |> Private.conceal_computation
;;

let%expect_test "map2_unfolded_constant_warnings" =
  test_start [%here];
  let c =
    let%arr a = Value.return 5
    and b = Value.return 3 in
    a + b
  in
  test_lint c;
  [%expect {|
    lib/bonsai/test/test_linter.ml:2:4: unfolded constant |}]
;;

let%expect_test "map2_optimized_gets_no_warnings" =
  test_start [%here];
  let c =
    let%arr a = Value.return 5
    and b = Value.return 3 in
    a + b
  in
  test_lint (constant_fold c);
  [%expect {| |}]
;;

let%expect_test "map2_two_unfolded_constants_warnings" =
  test_start [%here];
  let sum =
    let%map a = Value.return 5
    and b = Value.return 3 in
    a + b
  in
  let doubled =
    let%arr a = sum
    and b = sum in
    a + b
  in
  test_lint doubled;
  [%expect {| lib/bonsai/test/test_linter.ml:7:4: unfolded constant |}]
;;

let%expect_test "cutoff_unfolded_constants_warnings" =
  test_start [%here];
  let cutoff = return (Bonsai.Value.cutoff ~equal:( = ) (Value.return 3)) in
  test_lint cutoff;
  [%expect {| _none_:0:0: unfolded constant |}]
;;

let%expect_test "cutoff_optimized_gets_no_warnings" =
  test_start [%here];
  let cutoff = return (Value.cutoff ~equal:( = ) (Value.return 3)) in
  test_lint (constant_fold cutoff);
  [%expect {| |}]
;;

let%expect_test "sm1_with_const_input_gets_warning" =
  test_start [%here];
  let state_machine =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: Int.t]
      ~sexp_of_action:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _ _ _ -> 3)
      (Value.return 5)
  in
  test_lint state_machine;
  [%expect {| _none_:0:0: state_machine1 can be optimized to a state_machine0 |}]
;;

let%expect_test "sm1_optimized_gets_no_warnings" =
  test_start [%here];
  let state_machine =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: Int.t]
      ~sexp_of_action:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~default_model:0
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _ _ _ -> 3)
      (Value.return 5)
  in
  test_lint (constant_fold state_machine);
  [%expect {| |}]
;;

let%expect_test "map2_with_unfolded_constants_and_sm1_with_const_input_both_warned" =
  test_start [%here];
  let c =
    let%sub value, _inject =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Int.t]
        ~sexp_of_action:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        ~default_model:0
        ~apply_action:(fun ~inject:_ ~schedule_event:_ _ _ _ -> 3)
        (Value.return 5)
    in
    let%arr value = value
    and sum =
      let%map a = Value.return 5
      and b = Value.return 3 in
      a + b
    in
    value + sum
  in
  test_lint c;
  [%expect
    {|
    lib/bonsai/test/test_linter.ml:2:4: state_machine1 can be optimized to a state_machine0
    lib/bonsai/test/test_linter.ml:11:4: unfolded constant |}]
;;
