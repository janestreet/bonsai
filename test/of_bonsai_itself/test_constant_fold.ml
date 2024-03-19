open! Core
open! Import
open Bonsai.For_open
open Bonsai.Let_syntax
module Private = Bonsai.Private

let constant_fold computation graph =
  Private.handle graph ~f:computation
  |> Private.Constant_fold.constant_fold
  |> Private.perform graph
;;

let sexp_of_computation c =
  c
  |> Private.Skeleton.Computation.of_computation
  |> Private.Skeleton.Computation.sanitize_for_testing
  |> Private.Skeleton.Computation.minimal_sexp_of_t
;;

let print_computation c = print_s (sexp_of_computation (Private.top_level_handle c))

let constant_fold_and_assert_no_op computation =
  let lowered = Private.top_level_handle computation in
  let optimized = Private.Constant_fold.constant_fold lowered in
  let before_sexp = sexp_of_computation lowered in
  let after_sexp = sexp_of_computation optimized in
  match Sexp.equal before_sexp after_sexp with
  | true -> print_s before_sexp
  | false ->
    Expect_test_helpers_core.print_cr
      [%here]
      [%message "Expected before/after computations to be equal, but they are not"];
    Expect_test_patdiff.print_patdiff_s before_sexp after_sexp
;;

let constant_fold_and_diff computation =
  let lowered = Private.top_level_handle computation in
  let optimized = Private.Constant_fold.constant_fold lowered in
  let before_sexp = sexp_of_computation lowered in
  let after_sexp = sexp_of_computation optimized in
  Expect_test_patdiff.print_patdiff_s before_sexp after_sexp
;;

let%expect_test "map2_gets_folded" =
  let c =
    let%arr a = Value.return 5
    and b = Value.return 3 in
    a + b
  in
  print_computation c;
  [%expect
    {|
    (Return (
      value (
        Mapn (
          inputs ((
            Mapn (
              inputs (
                (Constant (id (Test 0)))
                (Constant (id (Test 1)))))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "Demonstrate: an unused opaque is not optimized away" =
  let c =
    let%sub _ = opaque_const 5 in
    Bonsai.const 3
  in
  print_computation c;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Sub
        (from (Return (value (Mapn (inputs ((Named (uid (Test 1)))))))))
        (via (Test 3))
        (into (Return (value (Constant (id (Test 4)))))))))
    |}];
  print_computation (constant_fold c);
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Sub
        (from (Return (value (Mapn (inputs ((Named (uid (Test 1)))))))))
        (via (Test 3))
        (into (Return (value (Constant (id (Test 4)))))))))
    |}]
;;

let%expect_test "opaque only used in a lazy not optimized away" =
  let c =
    let%sub a = opaque_const 5 in
    let%sub _ =
      opaque_computation ((Bonsai.lazy_ [@alert "-deprecated"]) (lazy (return a)))
    in
    Bonsai.const 3
  in
  constant_fold_and_assert_no_op c;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via (Test 3))
          (into (
            Switch
            (match_ (Mapn (inputs ((Named (uid (Test 3)))))))
            (arms ((Lazy (t ())) (Return (value Exception))))))))
        (via (Test 6))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
          (via (Test 8))
          (into (Return (value (Constant (id (Test 9)))))))))))
    |}]
;;

let%expect_test "opaque only used in a lazy not optimized away (but the lazy might)" =
  let c =
    let%sub a = opaque_const 5 in
    let%sub _ = (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (return a)) in
    Bonsai.const 3
  in
  print_computation c;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Sub
        (from (Lazy (t ())))
        (via (Test 2))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 2)))))))))
          (via (Test 4))
          (into (Return (value (Constant (id (Test 5)))))))))))
    |}];
  constant_fold_and_diff c;
  [%expect
    {|
    -1,12 +1,12
      (Sub
        (from (Return (value Incr)))
        (via (Test 1))
        (into (
          Sub
    -|    (from (Lazy (t ((Return (value (Named (uid (Test 1)))))))))
    +|    (from (Return (value (Named (uid (Test 1))))))
          (via (Test 2))
          (into (
            Sub
            (from (Return (value (Mapn (inputs ((Named (uid (Test 2)))))))))
            (via (Test 4))
            (into (Return (value (Constant (id (Test 5)))))))))))
    |}]
;;

let%expect_test "immediately-forced lazies are transparent to constant folding" =
  let c = (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5)) in
  print_computation c;
  [%expect {| (Lazy (t ())) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "nested immediate lazies are forced" =
  let c =
    (Bonsai.lazy_ [@alert "-deprecated"])
      (lazy ((Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5))))
  in
  print_computation c;
  [%expect {| (Lazy (t ())) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "lazies inside of a switch with dynamic input are preserved" =
  let c =
    if%sub opaque_const_value true
    then (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5))
    else assert false
  in
  constant_fold_and_assert_no_op c;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid (Test 1)))))))
        (arms ((Lazy (t ())) (Return (value Exception)))))))
    |}]
;;

let%expect_test "lazies inside of a switch with static input are forced" =
  let c =
    if%sub Value.return true
    then (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5))
    else assert false
  in
  print_computation c;
  [%expect
    {|
    (Switch
      (match_ (Mapn (inputs ((Constant (id (Test 0)))))))
      (arms ((Lazy (t ())) (Return (value Exception)))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "lazies inside of an assoc with static input are forced" =
  let c =
    Bonsai.assoc
      (module Int)
      (Value.return (Int.Map.of_alist_exn [ 1, (); 2, () ]))
      ~f:(fun _ _ -> (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5)))
  in
  print_computation c;
  [%expect
    {|
    (Assoc
      (map (Constant (id (Test 0))))
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (Lazy (t ()))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "lazies inside of an assoc with dynamic input are not forced" =
  let c =
    Bonsai.assoc
      (module Int)
      (opaque_const_value (Int.Map.of_alist_exn [ 1, (); 2, () ]))
      ~f:(fun _ _ -> (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5)))
  in
  constant_fold_and_assert_no_op c;
  [%expect
    {|
    (Assoc
      (map Incr)
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (Lazy (t ()))))
    |}]
;;

let%expect_test "map2_of_map2_of_constants_gets_folded" =
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
  print_computation doubled;
  [%expect
    {|
    (Return (
      value (
        Mapn (
          inputs ((
            Mapn (
              inputs (
                (Mapn (
                  inputs ((
                    Mapn (
                      inputs (
                        (Constant (id (Test 0)))
                        (Constant (id (Test 1)))))))))
                (Mapn (
                  inputs ((
                    Mapn (
                      inputs (
                        (Constant (id (Test 0)))
                        (Constant (id (Test 1)))))))))))))))))
    |}];
  print_computation (constant_fold doubled);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "cutoff" =
  let cutoff = return (Value.cutoff ~equal:( = ) (Value.return 3)) in
  print_computation cutoff;
  [%expect
    {|
    (Return (
      value (Cutoff (t (Constant (id (Test 0)))) (added_by_let_syntax false))))
    |}];
  print_computation (constant_fold cutoff);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "errors_propagate_but_are_not_thrown" =
  let c =
    let%arr a =
      let%map _ = Value.return 5 in
      raise (Failure "err")
    and () = Bonsai.Var.value (Bonsai.Var.create ()) in
    a
  in
  print_computation c;
  [%expect
    {|
    (Return (
      value (
        Mapn (
          inputs ((
            Mapn (inputs ((Mapn (inputs ((Constant (id (Test 0)))))) Incr))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value Exception)) |}]
;;

let%expect_test "cutoff gets folded away" =
  let c =
    return
      (Value.cutoff ~equal:Int.equal (Value.cutoff ~equal:Int.equal (Value.return 5)))
  in
  print_computation c;
  [%expect
    {|
    (Return (
      value (
        Cutoff
        (t (Cutoff (t (Constant (id (Test 0)))) (added_by_let_syntax false)))
        (added_by_let_syntax false))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "nested cutoffs get merged" =
  let c =
    return
      (Value.cutoff
         ~equal:Int.equal
         (Value.cutoff ~equal:Int.equal (opaque_const_value 5)))
  in
  print_computation c;
  [%expect
    {|
    (Return (
      value (
        Cutoff
        (t (
          Cutoff
          (t                   Incr)
          (added_by_let_syntax false)))
        (added_by_let_syntax false))))
    |}];
  print_computation (constant_fold c);
  [%expect
    {|
    (Return (
      value (
        Cutoff
        (t                   Incr)
        (added_by_let_syntax false))))
    |}]
;;

let%expect_test "state_machine1 with constant input is converted to state_machine0" =
  let c =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Int.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _model action _input ->
        action)
      (Value.return 5)
  in
  print_computation c;
  [%expect {| (Leaf1 (input (Constant (id (Test 0))))) |}];
  print_computation (constant_fold c);
  [%expect {| Leaf0 |}]
;;

let%expect_test "a constant input to assoc gets distributed to a bunch of subs" =
  let c =
    let%sub from_outside = opaque_const "testing" in
    Bonsai.assoc
      (module String)
      (Value.return (String.Map.of_alist_exn [ "hello", 0; "world", 5 ]))
      ~f:(fun k v ->
        let%arr k = k
        and v = v
        and from_outside = from_outside in
        k, v, from_outside)
  in
  print_computation c;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Assoc
        (map (Constant (id (Test 2))))
        (key_id  (Test 3))
        (cmp_id  (Test 4))
        (data_id (Test 5))
        (by (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 3)))
                      (Mapn (
                        inputs (
                          (Named (uid (Test 5)))
                          (Named (uid (Test 1))))))))))))))))))
    |}];
  constant_fold_and_diff c;
  [%expect
    {|
    -1,21 +1,42
      (Sub
        (from (Return (value Incr)))
        (via (Test 1))
        (into (
    -|    Assoc
    -|    (map (Constant (id (Test 2))))
    -|    (key_id  (Test 3))
    -|    (cmp_id  (Test 4))
    -|    (data_id (Test 5))
    -|    (by (
    +|    Sub
    +|    (from (
    +|      Sub
    +|      (from (
    +|        Sub
    +|        (from (
                Return (
                  value (
                    Mapn (
                      inputs ((
    -|              Mapn (
    -|                inputs (
    -|                  (Named (uid (Test 3)))
    -|                  (Mapn (
    +|                  Mapn (inputs ((Mapn (inputs ((Named (uid (Test 1)))))))))))))))
    +|        (via (Test 5))
    +|        (into (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))))
    +|      (via (Test 7))
    +|      (into (
    +|        Sub
    +|        (from (
    +|          Sub
    +|          (from (
    +|            Return (
    +|              value (
    +|                Mapn (
    +|                  inputs ((
    +|                    Mapn (inputs ((Mapn (inputs ((Named (uid (Test 1)))))))))))))))
    +|          (via (Test 8))
    +|          (into (Return (value (Mapn (inputs ((Named (uid (Test 8)))))))))))
    +|        (via (Test 10))
    +|        (into (
    +|          Return (
    +|            value (
    +|              Mapn (
    +|                inputs ((
    +|                  Mapn (
    -|                    inputs (
    -|                      (Named (uid (Test 5)))
    +|                    inputs (
    +|                      (Named (uid (Test 7)))
    -|                      (Named (uid (Test 1))))))))))))))))))
    +|                      (Named (uid (Test 10))))))))))))))))
    +|    (via (Test 13))
    +|    (into (Return (value (Mapn (inputs ((Named (uid (Test 13))))))))))))
    |}]
;;

let%expect_test "constant map + simplifiable assoc function => constant map" =
  let c =
    Bonsai.assoc
      (module String)
      (Value.return (String.Map.of_alist_exn [ "hello", 0; "world", 5 ]))
      ~f:(fun k v ->
        let%arr k = k
        and v = v in
        k, v)
  in
  print_computation c;
  [%expect
    {|
    (Assoc
      (map (Constant (id (Test 0))))
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (
        Return (
          value (
            Mapn (
              inputs ((
                Mapn (
                  inputs (
                    (Named (uid (Test 1)))
                    (Named (uid (Test 3)))))))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "a constant input with no external dependencies is folded into a constant"
  =
  let c =
    Bonsai.assoc
      (module String)
      (Value.return (String.Map.of_alist_exn [ "hello", 0; "world", 5 ]))
      ~f:(fun k v ->
        let%arr k = k
        and v = v in
        k, v)
  in
  print_computation c;
  [%expect
    {|
    (Assoc
      (map (Constant (id (Test 0))))
      (key_id  (Test 1))
      (cmp_id  (Test 2))
      (data_id (Test 3))
      (by (
        Return (
          value (
            Mapn (
              inputs ((
                Mapn (
                  inputs (
                    (Named (uid (Test 1)))
                    (Named (uid (Test 3)))))))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "a switch with constant input is optimized away" =
  let c =
    match%sub Value.return true with
    | true -> Bonsai.const "hello"
    | false -> Bonsai.const "world"
  in
  print_computation c;
  [%expect
    {|
    (Switch
      (match_ (Mapn (inputs ((Constant (id (Test 0)))))))
      (arms (
        (Return (value (Constant (id (Test 2)))))
        (Return (value (Constant (id (Test 3))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "an assert-false is caught (and then optimized away)" =
  let c =
    match%sub Value.return true with
    | true -> Bonsai.const "hello"
    | _ -> assert false
  in
  print_computation c;
  [%expect
    {|
    (Switch
      (match_ (Mapn (inputs ((Constant (id (Test 0)))))))
      (arms ((Return (value (Constant (id (Test 2))))) (Return (value Exception)))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id (Test 0))))) |}]
;;

let%expect_test "Static exception node on missing index of [Let_syntax.switch]." =
  let c =
    Bonsai.Let_syntax.Let_syntax.switch
      ~here:[%here]
      ~match_:(Value.return 1)
      ~branches:1
      ~with_:(function
      | 0 -> Bonsai.const "hi"
      | _ -> assert false)
  in
  print_computation c;
  [%expect
    {|
    (Switch
      (match_ (Constant (id (Test 0))))
      (arms ((Return (value (Constant (id (Test 1))))))))
    |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value Exception)) |}]
;;
