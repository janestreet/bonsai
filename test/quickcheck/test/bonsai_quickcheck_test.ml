open! Core
open Bonsai_test
open Bonsai_quickcheck
open Expect_test_helpers_core
module Q = Base_quickcheck

(***** QUICKCHECK TESTS ******)

let%expect_test _ =
  let random = Splittable_random.State.create (Random.State.make [| Random.bits () |]) in
  let recompute_view_on_random_computation () =
    let (T { unpacked; witness }) =
      Bonsai_quickcheck.to_packed_real_computation
        (Q.Generator.generate Top_level_computation.quickcheck_generator ~size:10 ~random)
    in
    let handle = Handle.create (witness_to_result_spec witness) unpacked in
    Handle.recompute_view handle
  in
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ();
  recompute_view_on_random_computation ()
;;

type f_opt =
  { f : 'a. 'a Bonsai.Private.Computation.t -> 'a Bonsai.Private.Computation.t }

let compare_optimization
      (fake_comp : 'a Computation.t)
      (witness : ('a, 'cmp) Witness.t)
      (f_opt : f_opt)
  =
  let fake_packed = Computation.T { unpacked = fake_comp; witness } in
  let (T { unpacked; witness }) = to_packed_real_computation fake_packed in
  let optimized =
    Bonsai.Private.reveal_computation unpacked
    |> f_opt.f
    |> Bonsai.Private.conceal_computation
  in
  let handle = Handle.create (witness_to_result_spec witness) unpacked in
  let opt_handle = Handle.create (witness_to_result_spec witness) optimized in
  let require_results_to_match () =
    assert (Witness.equal witness (Handle.result handle) (Handle.result opt_handle))
  in
  require_results_to_match ();
  let random = Splittable_random.State.create (Random.State.make [| Random.bits () |]) in
  let actions_generator = Option.value_exn (actions_generator witness) in
  let incoming_list =
    Q.Generator.generate ~size:15 ~random (Q.Generator.list_non_empty actions_generator)
  in
  clear_log ();
  Handle.do_actions handle incoming_list;
  let log = read_log () in
  clear_log ();
  Handle.do_actions opt_handle incoming_list;
  let opt_log = read_log () in
  assert (Sexp.equal log opt_log);
  require_results_to_match ()
;;

let%expect_test "quickcheck bad_opts" =
  require_does_raise [%here] (fun () ->
    Quickcheck.test
      (* The default trial count is either 1000 or 10_000 depending on the word size. This
         is too large, takes too long, and causes SIGTERM's while building. Manual testing
         shows 100 to be a good number.

         The default size count is also too large, meaning the recursion depth is too
         large and the tests SIGTERM. Manual testing shows 12 to be a good number here.
      *)
      ~sizes:(Sequence.cycle_list_exn (List.range 0 12 ~stop:`inclusive))
      ~trials:100
      Top_level_computation.quickcheck_generator
      ~f:(fun packed_comp ->
        let (T { unpacked; witness }) = packed_comp in
        let f_opt = { f = Bad_opts.bad_sub } in
        compare_optimization unpacked witness f_opt));
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
      (input _)
      (error
       "A Value.t introduced by the [let%sub] expression at TEST_FILENAME:0:0 was used outside of the scope that it was declared in. Make sure that you aren't storing it inside a ref.")) |}]
;;

let%expect_test "quickcheck flatten_values" =
  Quickcheck.test
    ~sizes:(Sequence.cycle_list_exn (List.range 0 12 ~stop:`inclusive))
    ~trials:100
    Top_level_computation.quickcheck_generator
    ~f:(fun comp ->
      let f_opt = { f = Bonsai.Private.Flatten_values.flatten_values } in
      let (T { unpacked; witness }) = comp in
      compare_optimization unpacked witness f_opt)
;;

(***** MANUAL SANITY CHECKS ******)

(* Function for checking manually generated fake computations in expect tests *)
let check_fake_computation witness unpacked =
  let packed = Computation.T { unpacked; witness } in
  let (T { unpacked; witness }) = to_packed_real_computation packed in
  let handle = Handle.create (witness_to_result_spec witness) unpacked in
  let print_c ~with_preprocess =
    let unpacked = Bonsai.Private.reveal_computation unpacked in
    unpacked
    |> (if with_preprocess then Bonsai.Private.pre_process else Fn.id)
    |> Bonsai.Private.Skeleton.Computation.of_computation
    |> Bonsai.Private.Skeleton.Computation.sanitize_for_testing
    |> Bonsai.Private.Skeleton.Computation.minimal_sexp_of_t
    |> sexp_to_string ~hide_positions:true
    |> print_string
  in
  let () =
    print_endline "Real computation:";
    print_c ~with_preprocess:false
  in
  let () =
    print_endline "Optimized:";
    print_c ~with_preprocess:true
  in
  print_endline "Result:";
  Handle.show handle
;;

let%expect_test "sanity check subst" =
  (* Check trivial Value.Return computation with a unit *)
  let fake_return = Computation.Return (Return ()) in
  let fake_subst = Computation.Subst (fake_return, Unit, fun value -> Return value) in
  check_fake_computation Unit fake_subst;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (Return (value (Named (uid 1))))))
    Optimized:
    (Return (value (Constant (id 0))))
    Result:
    () |}];
  (* Check Value.Map computation by converting return int to 2 *)
  let fake_return = Computation.Return (Return 1) in
  let fake_subst =
    Computation.Subst (fake_return, Int, fun value -> Return (Map (value, Int, Const 2)))
  in
  check_fake_computation Int fake_subst;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (Return (value (Mapn (inputs ((Named (uid 1)))))))))
    Optimized:
    (Return (value (Constant (id 0))))
    Result:
    2 |}]
;;

let%expect_test "sanity check switch" =
  (* Check trivial Return computation with a unit *)
  let fake_switch =
    Computation.Switch
      { either_value = Var (First ())
      ; first_witness = Unit
      ; second_witness = Int
      ; f_first = (fun value -> Return value)
      ; f_second = (fun _ -> Return (Return ()))
      }
  in
  check_fake_computation Unit fake_switch;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value Incr)))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Named (uid 5))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 7)
            (into (Return (value (Constant (id 8)))))))))))
    Optimized:
    (Sub
      (from (Return (value Incr)))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Named (uid 5))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 7)
            (into (Return (value (Constant (id 8)))))))))))
    Result:
    () |}];
  (* Check Value.Map function by adding 1 to a constant int *)
  let fake_switch =
    Computation.Switch
      { either_value = Var (Second 1)
      ; first_witness = Unit
      ; second_witness = Int
      ; f_first = (fun _ -> Return (Return 1))
      ; f_second = (fun value -> Return (Map (value, Int, Add_const 1)))
      }
  in
  check_fake_computation Int fake_switch;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value Incr)))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Constant (id 6))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 8)
            (into (Return (value (Mapn (inputs ((Named (uid 8))))))))))))))
    Optimized:
    (Sub
      (from (Return (value Incr)))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Constant (id 6))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 8)
            (into (Return (value (Mapn (inputs ((Named (uid 8))))))))))))))
    Result:
    2 |}]
;;

let%expect_test "sanity check switch (with full constant folding)" =
  (* Check trivial Return computation with a unit *)
  let fake_switch =
    Computation.Switch
      { either_value = Return (First ())
      ; first_witness = Unit
      ; second_witness = Int
      ; f_first = (fun value -> Return value)
      ; f_second = (fun _ -> Return (Return ()))
      }
  in
  check_fake_computation Unit fake_switch;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Named (uid 5))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 7)
            (into (Return (value (Constant (id 8)))))))))))
    Optimized:
    (Return (value (Constant (id 0))))
    Result:
    () |}];
  (* Check Value.Map function by adding 1 to a constant int *)
  let fake_switch =
    Computation.Switch
      { either_value = Return (Second 1)
      ; first_witness = Unit
      ; second_witness = Int
      ; f_first = (fun _ -> Return (Return 1))
      ; f_second = (fun value -> Return (Map (value, Int, Add_const 1)))
      }
  in
  check_fake_computation Int fake_switch;
  [%expect
    {|
    Real computation:
    (Sub
      (from (Return (value (Constant (id 0)))))
      (via 1)
      (into (
        Switch
        (match_ (Mapn (inputs ((Named (uid 1))))))
        (arms (
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 5)
            (into (Return (value (Constant (id 6))))))
          (Sub
            (from (Return (value (Mapn (inputs ((Named (uid 1))))))))
            (via 8)
            (into (Return (value (Mapn (inputs ((Named (uid 8))))))))))))))
    Optimized:
    (Return (value (Constant (id 0))))
    Result:
    2 |}]
;;

let%expect_test "sanity check assoc" =
  (* Add one to every int in the map *)
  let map_real_data = Map.of_alist_exn (module Int) [ 0, 0; 1, 1; 2, 2 ] in
  let fake_assoc =
    Computation.Assoc
      { map_value = Var map_real_data
      ; key_witness = Int
      ; value_witness = Int
      ; result_witness = Int
      ; f = (fun _key value -> Return (Map (value, Int, Add_const 1)))
      }
  in
  check_fake_computation (Map (Int, Int)) fake_assoc;
  [%expect
    {|
    Real computation:
    (Assoc
      (map     Incr)
      (key_id  1)
      (cmp_id  2)
      (data_id 3)
      (by (Return (value (Mapn (inputs ((Named (uid 3)))))))))
    Optimized:
    (Assoc_simpl (map Incr))
    Result:
    ((0 1) (1 2) (2 3)) |}]
;;

let%expect_test "sanity check state" =
  let fake_state = Computation.State { default_model = 1; default_witness = Int } in
  check_fake_computation (Tuple (Int, Effect_func Int)) fake_state;
  [%expect
    {|
    Real computation:
    Leaf0
    Optimized:
    Leaf0
    Result:
    (1 <fun>) |}]
;;

let%expect_test "sanity check Value.Both" =
  let fake_both =
    Computation.Return
      (Value.Both
         { first = Map (Return 1, Int, Add_const 1)
         ; first_witness = Int
         ; second = Return ()
         ; second_witness = Unit
         })
  in
  check_fake_computation (Tuple (Int, Unit)) fake_both;
  [%expect
    {|
    Real computation:
    (Return (
      value (
        Mapn (inputs ((Mapn (inputs ((Constant (id 1))))) (Constant (id 0)))))))
    Optimized:
    (Return (value (Constant (id 0))))
    Result:
    (2 ()) |}]
;;

let%expect_test "manual check bad_sub" =
  (* Check trivial Value.Return computation with a unit *)
  let fake_return = Computation.Return (Var 5) in
  let fake_subst = Computation.Subst (fake_return, Int, fun value -> Return value) in
  let f_opt = { f = Bad_opts.bad_sub } in
  require_does_raise [%here] (fun () -> compare_optimization fake_subst Int f_opt);
  [%expect
    {|
    "A Value.t introduced by the [let%sub] expression at TEST_FILENAME:0:0 was used outside of the scope that it was declared in. Make sure that you aren't storing it inside a ref." |}];
  (* Check Value.Map computation by adding 1 to every value *)
  let fake_return = Computation.Return (Var 1) in
  let fake_subst =
    Computation.Subst
      (fake_return, Int, fun value -> Return (Map (value, Int, Add_const 1)))
  in
  require_does_raise [%here] (fun () -> compare_optimization fake_subst Int f_opt);
  [%expect
    {|
    "A Value.t introduced by the [let%sub] expression at TEST_FILENAME:0:0 was used outside of the scope that it was declared in. Make sure that you aren't storing it inside a ref." |}]
;;
