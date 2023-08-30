open! Core
open! Import
open Bonsai.For_open
open Bonsai.Let_syntax
module Private = Bonsai.Private

let constant_fold computation =
  computation
  |> Private.reveal_computation
  |> Private.Constant_fold.constant_fold
  |> Private.conceal_computation
;;

let sexp_of_computation c =
  c
  |> Private.reveal_computation
  |> Private.Skeleton.Computation.of_computation
  |> Private.Skeleton.Computation.sanitize_for_testing
  |> Private.Skeleton.Computation.minimal_sexp_of_t
;;

let print_computation c = print_s (sexp_of_computation c)

let constant_fold_and_assert_no_op computation =
  let after_computation = constant_fold computation in
  let before_sexp = sexp_of_computation computation in
  let after_sexp = sexp_of_computation after_computation in
  match Sexp.equal before_sexp after_sexp with
  | true -> print_s before_sexp
  | false ->
    Expect_test_helpers_core.print_cr
      [%here]
      [%message "Expected before/after computations to be equal, but they are not"];
    Expect_test_patdiff.print_patdiff_s before_sexp after_sexp
;;

let constant_fold_and_diff computation =
  let after_computation = constant_fold computation in
  let before_sexp = sexp_of_computation computation in
  let after_sexp = sexp_of_computation after_computation in
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
                (Constant (id 0))
                (Constant (id 1)))))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
      (via 1)
      (into (
        Sub
        (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
        (via 3)
        (into (Return (value (Constant (id 4)))))))) |}];
  print_computation (constant_fold c);
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via 1)
      (into (
        Sub
        (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
        (via 3)
        (into (Return (value (Constant (id 4)))))))) |}]
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
      (via 1)
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via 3)
          (into (
            Switch
            (match_ (Mapn (inputs ((Named (uid 3))))))
            (arms ((Lazy (t ())) (Return (value Exception))))))))
        (via 7)
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid 7))))))))
          (via 9)
          (into (Return (value (Constant (id 10)))))))))) |}]
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
      (via 1)
      (into (
        Sub
        (from (Lazy (t ())))
        (via 2)
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid 2))))))))
          (via 4)
          (into (Return (value (Constant (id 5)))))))))) |}];
  constant_fold_and_diff c;
  [%expect
    {|
    -1,12 +1,12
      (Sub
        (from (Return (value Incr)))
        (via 1)
        (into (
          Sub
    -|    (from (Lazy (t ((Return (value (Named (uid 1))))))))
    +|    (from (Return (value (Named (uid 1)))))
          (via 2)
          (into (
            Sub
            (from (Return (value (Mapn (inputs ((Named (uid 2))))))))
            (via 4)
            (into (Return (value (Constant (id 5)))))))))) |}]
;;

let%expect_test "immediately-forced lazies are transparent to constant folding" =
  let c = (Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5)) in
  print_computation c;
  [%expect {| (Lazy (t ())) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
;;

let%expect_test "nested immediate lazies are forced" =
  let c =
    (Bonsai.lazy_ [@alert "-deprecated"])
      (lazy ((Bonsai.lazy_ [@alert "-deprecated"]) (lazy (Bonsai.const 5))))
  in
  print_computation c;
  [%expect {| (Lazy (t ())) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
        (via 1)
        (into (
          Switch
          (match_ (Mapn (inputs ((Named (uid 1))))))
          (arms ((Lazy (t ())) (Return (value Exception))))))) |}]
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
      (Sub
        (from (Return (value (Constant (id 0)))))
        (via 1)
        (into (
          Switch
          (match_ (Mapn (inputs ((Named (uid 1))))))
          (arms ((Lazy (t ())) (Return (value Exception))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
        (map (Constant (id 0)))
        (key_id  1)
        (cmp_id  2)
        (data_id 3)
        (by (Lazy (t ())))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
      (map     Incr)
      (key_id  1)
      (cmp_id  2)
      (data_id 3)
      (by (Lazy (t ())))) |}]
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
                        (Constant (id 0))
                        (Constant (id 1))))))))
                (Mapn (
                  inputs ((
                    Mapn (
                      inputs (
                        (Constant (id 0))
                        (Constant (id 1)))))))))))))))) |}];
  print_computation (constant_fold doubled);
  [%expect {|
    (Return (value (Constant (id 0)))) |}]
;;

let%expect_test "cutoff" =
  let cutoff = return (Value.cutoff ~equal:( = ) (Value.return 3)) in
  print_computation cutoff;
  [%expect
    {| (Return (value (Cutoff (t (Constant (id 0))) (added_by_let_syntax false)))) |}];
  print_computation (constant_fold cutoff);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
        Mapn (inputs ((Mapn (inputs ((Mapn (inputs ((Constant (id 0))))) Incr)))))))) |}];
  print_computation (constant_fold c);
  [%expect {|
    (Return (value Exception)) |}]
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
          (t (Cutoff (t (Constant (id 0))) (added_by_let_syntax false)))
          (added_by_let_syntax false)))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
        (added_by_let_syntax false)))) |}];
  print_computation (constant_fold c);
  [%expect
    {|
    (Return (
      value (
        Cutoff
        (t                   Incr)
        (added_by_let_syntax false)))) |}]
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
  [%expect {| (Leaf1 (input (Constant (id 0)))) |}];
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
      (via 1)
      (into (
        Assoc
        (map (Constant (id 3)))
        (key_id  4)
        (cmp_id  5)
        (data_id 6)
        (by (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 4))
                      (Mapn (
                        inputs (
                          (Named (uid 6))
                          (Named (uid 1))))))))))))))))) |}];
  constant_fold_and_diff c;
  [%expect
    {|
    -1,21 +1,40
      (Sub
        (from (Return (value Incr)))
        (via 1)
        (into (
    -|    Assoc
    -|    (map (Constant (id 3)))
    -|    (key_id  4)
    -|    (cmp_id  5)
    -|    (data_id 6)
    -|    (by (
    +|    Sub
    +|    (from (
    +|      Sub
    +|      (from (
    +|        Sub
    +|        (from (
    +|          Return (
    +|            value (
    +|              Mapn (
    +|                inputs ((Mapn (inputs ((Mapn (inputs ((Named (uid 1))))))))))))))
    +|        (via 16)
    +|        (into (Return (value (Mapn (inputs ((Named (uid 16))))))))))
    +|      (via 20)
    +|      (into (
    +|        Sub
    +|        (from (
    +|          Sub
    +|          (from (
    +|            Return (
    +|              value (
    +|                Mapn (
    +|                  inputs ((Mapn (inputs ((Mapn (inputs ((Named (uid 1))))))))))))))
    +|          (via 18)
    +|          (into (Return (value (Mapn (inputs ((Named (uid 18))))))))))
    +|        (via 21)
    +|        (into (
                Return (
                  value (
                    Mapn (
                      inputs ((
                        Mapn (
                          inputs (
    -|                  (Named (uid 4))
    -|                  (Mapn (
    -|                    inputs (
    -|                      (Named (uid 6))
    -|                      (Named (uid 1)))))))))))))))))
    +|                      (Named (uid 20))
    +|                      (Named (uid 21)))))))))))))))
    +|    (via 24)
    +|    (into (Return (value (Mapn (inputs ((Named (uid 24))))))))))) |}]
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
      (map (Constant (id 0)))
      (key_id  1)
      (cmp_id  2)
      (data_id 3)
      (by (
        Return (
          value (
            Mapn (
              inputs ((
                Mapn (
                  inputs (
                    (Named (uid 1))
                    (Named (uid 3)))))))))))) |}];
  print_computation (constant_fold c);
  [%expect {|
    (Return (value (Constant (id 0)))) |}]
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
      (map (Constant (id 0)))
      (key_id  1)
      (cmp_id  2)
      (data_id 3)
      (by (
        Return (
          value (
            Mapn (
              inputs ((
                Mapn (
                  inputs (
                    (Named (uid 1))
                    (Named (uid 3)))))))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Return (value (Constant (id 4)))) (Return (value (Constant (id 5))))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms ((Return (value (Constant (id 4)))) (Return (value Exception))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value (Constant (id 0)))) |}]
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
      (match_ (Constant (id 0)))
      (arms ((Return (value (Constant (id 2))))))) |}];
  print_computation (constant_fold c);
  [%expect {| (Return (value Exception)) |}]
;;
