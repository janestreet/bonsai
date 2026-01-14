open Core
open Balance_list_tree

let test ~n ~list_len =
  let list = List.init list_len ~f:Fn.id in
  match balance ~n list with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok result -> print_s [%sexp (result : int t)]
;;

let%expect_test "special case of ~n=1, list_len=1" =
  test ~n:1 ~list_len:1;
  [%expect {| (Leaf 0) |}]
;;

let%expect_test "basic behavior" =
  test ~n:2 ~list_len:5;
  [%expect
    {|
    (Node
     ((Node ((Node ((Leaf 0) (Leaf 1))) (Node ((Leaf 2) (Leaf 3))))) (Leaf 4)))
    |}];
  test ~n:5 ~list_len:20;
  [%expect
    {|
    (Node
     ((Node ((Leaf 0) (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4)))
      (Node ((Leaf 5) (Leaf 6) (Leaf 7) (Leaf 8) (Leaf 9)))
      (Node ((Leaf 10) (Leaf 11) (Leaf 12) (Leaf 13) (Leaf 14)))
      (Node ((Leaf 15) (Leaf 16) (Leaf 17) (Leaf 18) (Leaf 19)))))
    |}]
;;

let%expect_test "(mod n list_len = mod n (-1))" =
  test ~n:2 ~list_len:16;
  [%expect
    {|
    (Node
     ((Node
       ((Node ((Node ((Leaf 0) (Leaf 1))) (Node ((Leaf 2) (Leaf 3)))))
        (Node ((Node ((Leaf 4) (Leaf 5))) (Node ((Leaf 6) (Leaf 7)))))))
      (Node
       ((Node ((Node ((Leaf 8) (Leaf 9))) (Node ((Leaf 10) (Leaf 11)))))
        (Node ((Node ((Leaf 12) (Leaf 13))) (Node ((Leaf 14) (Leaf 15)))))))))
    |}];
  test ~n:3 ~list_len:5;
  [%expect {| (Node ((Node ((Leaf 0) (Leaf 1) (Leaf 2))) (Node ((Leaf 3) (Leaf 4))))) |}];
  test ~n:3 ~list_len:11;
  [%expect
    {|
    (Node
     ((Node
       ((Node ((Leaf 0) (Leaf 1) (Leaf 2))) (Node ((Leaf 3) (Leaf 4) (Leaf 5)))
        (Node ((Leaf 6) (Leaf 7) (Leaf 8)))))
      (Node ((Leaf 9) (Leaf 10)))))
    |}];
  test ~n:7 ~list_len:13;
  [%expect
    {|
    (Node
     ((Node ((Leaf 0) (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4) (Leaf 5) (Leaf 6)))
      (Node ((Leaf 7) (Leaf 8) (Leaf 9) (Leaf 10) (Leaf 11) (Leaf 12)))))
    |}];
  test ~n:5 ~list_len:19;
  [%expect
    {|
    (Node
     ((Node ((Leaf 0) (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4)))
      (Node ((Leaf 5) (Leaf 6) (Leaf 7) (Leaf 8) (Leaf 9)))
      (Node ((Leaf 10) (Leaf 11) (Leaf 12) (Leaf 13) (Leaf 14)))
      (Node ((Leaf 15) (Leaf 16) (Leaf 17) (Leaf 18)))))
    |}]
;;

let%expect_test "errors" =
  test ~n:(-1) ~list_len:5;
  [%expect {| (error "expand_letn: n must be positive") |}];
  test ~n:0 ~list_len:5;
  [%expect {| (error "expand_letn: n must be positive") |}];
  test ~n:5 ~list_len:0;
  [%expect {| (error "expand_letn: list of bindings must be non-empty") |}]
;;

let%expect_test "regression" =
  test ~n:7 ~list_len:50;
  [%expect
    {|
    (Node
     ((Node
       ((Node ((Leaf 0) (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4) (Leaf 5) (Leaf 6)))
        (Node
         ((Leaf 7) (Leaf 8) (Leaf 9) (Leaf 10) (Leaf 11) (Leaf 12) (Leaf 13)))
        (Node
         ((Leaf 14) (Leaf 15) (Leaf 16) (Leaf 17) (Leaf 18) (Leaf 19) (Leaf 20)))
        (Node
         ((Leaf 21) (Leaf 22) (Leaf 23) (Leaf 24) (Leaf 25) (Leaf 26) (Leaf 27)))
        (Node
         ((Leaf 28) (Leaf 29) (Leaf 30) (Leaf 31) (Leaf 32) (Leaf 33) (Leaf 34)))
        (Node
         ((Leaf 35) (Leaf 36) (Leaf 37) (Leaf 38) (Leaf 39) (Leaf 40) (Leaf 41)))
        (Node
         ((Leaf 42) (Leaf 43) (Leaf 44) (Leaf 45) (Leaf 46) (Leaf 47) (Leaf 48)))))
      (Leaf 49)))
    |}]
;;

let%expect_test "balancer is balanced, doesn't have more than `n` children per node, and \
                 has all leaves exactly once. Also, leaves preserve order. "
  =
  let%quick_test prop
    (n : (int[@generator Int.gen_uniform_incl 0 15]))
    (list_len : (int[@generator Int.gen_uniform_incl 0 1_000]))
    =
    let list = List.init list_len ~f:Fn.id in
    match balance ~n list, n, list with
    | Error _, _, [] | Error _, 0, _ -> ()
    | Error _, 1, ls when List.length ls > 1 -> ()
    | Error err, _, _ ->
      Error.raise_s
        [%message "Balancer errored" (err : Error.t) (n : int) (List.length list : int)]
    | Ok balanced, _, _ ->
      let last_visited_leaf = ref (-1) in
      let rec traverse ~depth = function
        | Leaf v ->
          assert (v = !last_visited_leaf + 1);
          last_visited_leaf := v
        | Node children ->
          (match children with
           | [ Leaf _ ] -> raise_s [%message "Found an unflattened node!"]
           | _ -> ());
          assert (Nonempty_list.length children <= n);
          Nonempty_list.fold children ~init:`Seen_no_leaves ~f:(fun acc child ->
            traverse ~depth:(depth + 1) child;
            match acc, child with
            | `Seen_no_leaves, Node _ -> `Seen_no_leaves
            | `Seen_no_leaves, Leaf _ -> `Seen_leaves
            | `Seen_leaves, Node _ -> failwith "All nodes must be before all leaves"
            | `Seen_leaves, Leaf _ -> `Seen_leaves)
          |> Fn.ignore
      in
      traverse ~depth:0 balanced;
      assert (List.length list = !last_visited_leaf + 1);
      [%expect {| |}]
  in
  ()
;;
