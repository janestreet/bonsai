open! Core
open Bonsai_web_test
open Bonsai_test_of_bonsai_itself.Big_computation_regression_util

module Bonsai_cont = struct
  include Bonsai.Cont
  module Private = Bonsai.Private
end

let sexp_of_computation
  : type a. ?optimize:bool -> (Bonsai_cont.graph -> a Bonsai_cont.t) -> Sexp.t
  =
  fun ?(optimize = true) c ->
  Bonsai_cont.Private.top_level_handle c
  |> (if optimize then Bonsai_cont.Private.pre_process else Fn.id)
  |> Bonsai_cont.Private.Skeleton.Computation.of_computation
  |> Bonsai_cont.Private.Skeleton.Computation.sanitize_for_testing
  |> Bonsai_cont.Private.Skeleton.Computation.minimal_sexp_of_t
;;

let%test_module "Comparing graph structure." =
  (module struct
    let%expect_test "Proc Syntax" =
      print_s (sexp_of_computation (For_proc.basic ~height:2 ~width:2));
      [%expect
        {|
        (Sub
         (from
          (Sub (from Leaf0) (via (Test 0))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 2))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 4))
               (into
                (Sub (from Leaf0) (via (Test 5))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 7))
                   (into
                    (Sub
                     (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                     (via (Test 9))
                     (into
                      (Return
                       (value
                        (Mapn
                         (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
         (via (Test 11))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
           (via (Test 13))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
             (via (Test 15))
             (into
              (Sub
               (from
                (Sub (from Path) (via (Test 16))
                 (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
               (via (Test 18))
               (into
                (Sub
                 (from
                  (Sub
                   (from
                    (Sub (from Leaf0) (via (Test 19))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                       (via (Test 21))
                       (into
                        (Sub
                         (from
                          (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                         (via (Test 23))
                         (into
                          (Sub (from Leaf0) (via (Test 24))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 24)))))))))
                             (via (Test 26))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 24)))))))))
                               (via (Test 28))
                               (into
                                (Return
                                 (value
                                  (Mapn
                                   (inputs
                                    ((Named (uid (Test 26))) (Named (uid (Test 28)))))))))))))))))))))
                   (via (Test 30))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                     (via (Test 32))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                       (via (Test 34))
                       (into
                        (Sub
                         (from
                          (Sub (from Path) (via (Test 16))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                         (via (Test 35))
                         (into
                          (Sub
                           (from
                            (Sub
                             (from
                              (Sub (from Path) (via (Test 16))
                               (into
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                             (via (Test 36))
                             (into
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 36)))))))))))
                           (via (Test 38))
                           (into
                            (Return
                             (value
                              (Mapn
                               (inputs
                                ((Named (uid (Test 38))) (Named (uid (Test 35)))))))))))))))))))
                 (via (Test 40))
                 (into
                  (Return
                   (value
                    (Mapn (inputs ((Named (uid (Test 40))) (Named (uid (Test 18)))))))))))))))))) |}]
    ;;

    let%expect_test "Cont Syntax" =
      print_s (sexp_of_computation (For_cont.basic ~height:2 ~width:2));
      [%expect
        {|
        (Sub (from Leaf0) (via (Test 0))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 4))
             (into
              (Sub (from Leaf0) (via (Test 5))
               (into
                (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 7))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 9))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 10))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                     (via (Test 12))
                     (into
                      (Sub (from Leaf0) (via (Test 13))
                       (into
                        (Sub
                         (from
                          (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                         (via (Test 15))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                           (via (Test 17))
                           (into
                            (Sub (from Leaf0) (via (Test 18))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 18)))))))))
                               (via (Test 20))
                               (into
                                (Sub
                                 (from
                                  (Return
                                   (value (Mapn (inputs ((Named (uid (Test 18)))))))))
                                 (via (Test 22))
                                 (into
                                  (Sub
                                   (from
                                    (Sub (from Path) (via (Test 10))
                                     (into
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 10)))))))))))
                                   (via (Test 23))
                                   (into
                                    (Sub
                                     (from
                                      (Sub (from Path) (via (Test 10))
                                       (into
                                        (Return
                                         (value
                                          (Mapn (inputs ((Named (uid (Test 10)))))))))))
                                     (via (Test 24))
                                     (into
                                      (Sub
                                       (from
                                        (Return
                                         (value
                                          (Mapn (inputs ((Named (uid (Test 24)))))))))
                                       (via (Test 26))
                                       (into
                                        (Sub
                                         (from
                                          (Return
                                           (value
                                            (Mapn
                                             (inputs
                                              ((Named (uid (Test 26)))
                                               (Named (uid (Test 23)))))))))
                                         (via (Test 28))
                                         (into
                                          (Sub
                                           (from
                                            (Return
                                             (value
                                              (Mapn
                                               (inputs ((Named (uid (Test 28)))))))))
                                           (via (Test 30))
                                           (into
                                            (Sub
                                             (from
                                              (Return
                                               (value
                                                (Mapn
                                                 (inputs
                                                  ((Named (uid (Test 30)))
                                                   (Named (uid (Test 12)))))))))
                                             (via (Test 32))
                                             (into
                                              (Return
                                               (value
                                                (Mapn
                                                 (inputs ((Named (uid (Test 32))))))))))))))))))))))))))))))))))))))))))))))
        |}]
    ;;
  end)
;;

let%test_module "With Assocs." =
  (module struct
    let%expect_test "Proc Syntax" =
      print_s (sexp_of_computation (For_proc.with_assoc ~height:1 ~width:2 ~num_assocs:5));
      [%expect
        {|
        (Sub
         (from
          (Sub
           (from
            (Sub (from Leaf0) (via (Test 0))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 2))
               (into
                (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
                 (via (Test 4))
                 (into
                  (Sub (from Leaf0) (via (Test 5))
                   (into
                    (Sub
                     (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                     (via (Test 7))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                       (via (Test 9))
                       (into
                        (Return
                         (value
                          (Mapn
                           (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
           (via (Test 11))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
             (via (Test 13))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
               (via (Test 15))
               (into
                (Sub
                 (from
                  (Sub (from Path) (via (Test 16))
                   (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                 (via (Test 18))
                 (into
                  (Sub
                   (from
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 16))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                     (via (Test 19))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))))
                   (via (Test 21))
                   (into
                    (Return
                     (value
                      (Mapn
                       (inputs ((Named (uid (Test 21))) (Named (uid (Test 18)))))))))))))))))))
         (via (Test 23))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 23)))))))))
           (via (Test 25))
           (into
            (Sub (from (Leaf_incr (input (Named (uid (Test 25)))))) (via (Test 26))
             (into
              (Assoc (map (Named (uid (Test 26)))) (key_id (Test 27))
               (cmp_id (Test 28)) (data_id (Test 29))
               (by
                (Sub
                 (from
                  (Sub (from Leaf0) (via (Test 30))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                     (via (Test 32))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                       (via (Test 34))
                       (into
                        (Sub (from Leaf0) (via (Test 35))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 35)))))))))
                           (via (Test 37))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 35)))))))))
                             (via (Test 39))
                             (into
                              (Return
                               (value
                                (Mapn
                                 (inputs
                                  ((Named (uid (Test 37))) (Named (uid (Test 39)))))))))))))))))))))
                 (via (Test 41))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 41)))))))))
                   (via (Test 43))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 41)))))))))
                     (via (Test 45))
                     (into
                      (Sub
                       (from
                        (Sub (from Path) (via (Test 16))
                         (into
                          (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                       (via (Test 46))
                       (into
                        (Sub
                         (from
                          (Sub
                           (from
                            (Sub (from Path) (via (Test 16))
                             (into
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                           (via (Test 47))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 47)))))))))))
                         (via (Test 49))
                         (into
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 49))) (Named (uid (Test 46)))))))))))))))))))))))))) |}]
    ;;

    let%expect_test "Cont Syntax" =
      print_s (sexp_of_computation (For_cont.with_assoc ~height:1 ~width:2 ~num_assocs:5));
      [%expect
        {|
        (Sub (from Leaf0) (via (Test 0))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 4))
             (into
              (Sub (from Leaf0) (via (Test 5))
               (into
                (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 7))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 9))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 10))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                     (via (Test 12))
                     (into
                      (Sub
                       (from
                        (Sub (from Path) (via (Test 10))
                         (into
                          (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                       (via (Test 13))
                       (into
                        (Sub
                         (from
                          (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                         (via (Test 15))
                         (into
                          (Sub
                           (from
                            (Return
                             (value
                              (Mapn
                               (inputs
                                ((Named (uid (Test 15))) (Named (uid (Test 12)))))))))
                           (via (Test 17))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                             (via (Test 19))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                               (via (Test 21))
                               (into
                                (Sub
                                 (from (Leaf_incr (input (Named (uid (Test 21))))))
                                 (via (Test 22))
                                 (into
                                  (Assoc (map (Named (uid (Test 22))))
                                   (key_id (Test 23)) (cmp_id (Test 24))
                                   (data_id (Test 25))
                                   (by
                                    (Sub (from Leaf0) (via (Test 26))
                                     (into
                                      (Sub
                                       (from
                                        (Return
                                         (value
                                          (Mapn (inputs ((Named (uid (Test 26)))))))))
                                       (via (Test 28))
                                       (into
                                        (Sub
                                         (from
                                          (Return
                                           (value
                                            (Mapn (inputs ((Named (uid (Test 26)))))))))
                                         (via (Test 30))
                                         (into
                                          (Sub (from Leaf0) (via (Test 31))
                                           (into
                                            (Sub
                                             (from
                                              (Return
                                               (value
                                                (Mapn
                                                 (inputs ((Named (uid (Test 31)))))))))
                                             (via (Test 33))
                                             (into
                                              (Sub
                                               (from
                                                (Return
                                                 (value
                                                  (Mapn
                                                   (inputs ((Named (uid (Test 31)))))))))
                                               (via (Test 35))
                                               (into
                                                (Sub
                                                 (from
                                                  (Sub (from Path) (via (Test 10))
                                                   (into
                                                    (Return
                                                     (value
                                                      (Mapn
                                                       (inputs
                                                        ((Named (uid (Test 10)))))))))))
                                                 (via (Test 36))
                                                 (into
                                                  (Sub
                                                   (from
                                                    (Sub (from Path) (via (Test 10))
                                                     (into
                                                      (Return
                                                       (value
                                                        (Mapn
                                                         (inputs
                                                          ((Named (uid (Test 10)))))))))))
                                                   (via (Test 37))
                                                   (into
                                                    (Sub
                                                     (from
                                                      (Return
                                                       (value
                                                        (Mapn
                                                         (inputs
                                                          ((Named (uid (Test 37)))))))))
                                                     (via (Test 39))
                                                     (into
                                                      (Sub
                                                       (from
                                                        (Return
                                                         (value
                                                          (Mapn
                                                           (inputs
                                                            ((Named (uid (Test 39)))
                                                             (Named (uid (Test 36)))))))))
                                                       (via (Test 41))
                                                       (into
                                                        (Return
                                                         (value
                                                          (Mapn
                                                           (inputs
                                                            ((Named (uid (Test 41))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        |}]
    ;;
  end)
;;

let%test_module "With match%sub." =
  (module struct
    let%expect_test "Proc Syntax" =
      print_s (sexp_of_computation (For_proc.with_switch ~height:1 ~width:2));
      [%expect
        {|
        (Sub
         (from
          (Sub
           (from
            (Sub (from Leaf0) (via (Test 0))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 2))
               (into
                (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
                 (via (Test 4))
                 (into
                  (Sub (from Leaf0) (via (Test 5))
                   (into
                    (Sub
                     (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                     (via (Test 7))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                       (via (Test 9))
                       (into
                        (Return
                         (value
                          (Mapn
                           (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
           (via (Test 11))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
             (via (Test 13))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
               (via (Test 15))
               (into
                (Sub
                 (from
                  (Sub (from Path) (via (Test 16))
                   (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                 (via (Test 18))
                 (into
                  (Sub
                   (from
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 16))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                     (via (Test 19))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))))
                   (via (Test 21))
                   (into
                    (Return
                     (value
                      (Mapn
                       (inputs ((Named (uid (Test 21))) (Named (uid (Test 18)))))))))))))))))))
         (via (Test 23))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 23)))))))))
           (via (Test 25))
           (into
            (Switch (match_ (Mapn (inputs ((Named (uid (Test 25)))))))
             (arms
              ((Return (value (Constant (id (Test 27)))))
               (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 25)))))))))
                (via (Test 29))
                (into (Return (value (Mapn (inputs ((Named (uid (Test 29))))))))))
               (Sub
                (from
                 (Sub
                  (from
                   (Sub (from Leaf0) (via (Test 31))
                    (into
                     (Sub
                      (from
                       (Return (value (Mapn (inputs ((Named (uid (Test 31)))))))))
                      (via (Test 33))
                      (into
                       (Sub
                        (from
                         (Return (value (Mapn (inputs ((Named (uid (Test 31)))))))))
                        (via (Test 35))
                        (into
                         (Sub (from Leaf0) (via (Test 36))
                          (into
                           (Sub
                            (from
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 36)))))))))
                            (via (Test 38))
                            (into
                             (Sub
                              (from
                               (Return
                                (value (Mapn (inputs ((Named (uid (Test 36)))))))))
                              (via (Test 40))
                              (into
                               (Return
                                (value
                                 (Mapn
                                  (inputs
                                   ((Named (uid (Test 38))) (Named (uid (Test 40)))))))))))))))))))))
                  (via (Test 42))
                  (into
                   (Sub
                    (from (Return (value (Mapn (inputs ((Named (uid (Test 42)))))))))
                    (via (Test 44))
                    (into
                     (Sub
                      (from
                       (Return (value (Mapn (inputs ((Named (uid (Test 42)))))))))
                      (via (Test 46))
                      (into
                       (Sub
                        (from
                         (Sub (from Path) (via (Test 16))
                          (into
                           (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                        (via (Test 47))
                        (into
                         (Sub
                          (from
                           (Sub
                            (from
                             (Sub (from Path) (via (Test 16))
                              (into
                               (Return
                                (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
                            (via (Test 48))
                            (into
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 48)))))))))))
                          (via (Test 50))
                          (into
                           (Return
                            (value
                             (Mapn
                              (inputs
                               ((Named (uid (Test 50))) (Named (uid (Test 47)))))))))))))))))))
                (via (Test 52))
                (into (Return (value (Mapn (inputs ((Named (uid (Test 52))))))))))))))))) |}]
    ;;

    let%expect_test "Cont Syntax" =
      print_s (sexp_of_computation (For_cont.with_switch ~height:1 ~width:2));
      [%expect
        {|
        (Sub (from Leaf0) (via (Test 0))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 4))
             (into
              (Sub (from Leaf0) (via (Test 5))
               (into
                (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 7))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 9))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 10))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                     (via (Test 12))
                     (into
                      (Sub
                       (from
                        (Sub (from Path) (via (Test 10))
                         (into
                          (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                       (via (Test 13))
                       (into
                        (Sub
                         (from
                          (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                         (via (Test 15))
                         (into
                          (Sub
                           (from
                            (Return
                             (value
                              (Mapn
                               (inputs
                                ((Named (uid (Test 15))) (Named (uid (Test 12)))))))))
                           (via (Test 17))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                             (via (Test 19))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                               (via (Test 21))
                               (into
                                (Sub
                                 (from
                                  (Return
                                   (value (Mapn (inputs ((Named (uid (Test 21)))))))))
                                 (via (Test 23))
                                 (into
                                  (Switch (match_ (Named (uid (Test 23))))
                                   (arms
                                    ((Return (value (Constant (id (Test 24)))))
                                     (Sub
                                      (from
                                       (Return
                                        (value
                                         (Mapn (inputs ((Named (uid (Test 21)))))))))
                                      (via (Test 26))
                                      (into
                                       (Return
                                        (value
                                         (Mapn (inputs ((Named (uid (Test 26))))))))))
                                     (Sub (from Leaf0) (via (Test 28))
                                      (into
                                       (Sub
                                        (from
                                         (Return
                                          (value
                                           (Mapn (inputs ((Named (uid (Test 28)))))))))
                                        (via (Test 30))
                                        (into
                                         (Sub
                                          (from
                                           (Return
                                            (value
                                             (Mapn
                                              (inputs ((Named (uid (Test 28)))))))))
                                          (via (Test 32))
                                          (into
                                           (Sub (from Leaf0) (via (Test 33))
                                            (into
                                             (Sub
                                              (from
                                               (Return
                                                (value
                                                 (Mapn
                                                  (inputs ((Named (uid (Test 33)))))))))
                                              (via (Test 35))
                                              (into
                                               (Sub
                                                (from
                                                 (Return
                                                  (value
                                                   (Mapn
                                                    (inputs
                                                     ((Named (uid (Test 33)))))))))
                                                (via (Test 37))
                                                (into
                                                 (Sub
                                                  (from
                                                   (Sub (from Path) (via (Test 10))
                                                    (into
                                                     (Return
                                                      (value
                                                       (Mapn
                                                        (inputs
                                                         ((Named (uid (Test 10)))))))))))
                                                  (via (Test 38))
                                                  (into
                                                   (Sub
                                                    (from
                                                     (Sub (from Path) (via (Test 10))
                                                      (into
                                                       (Return
                                                        (value
                                                         (Mapn
                                                          (inputs
                                                           ((Named (uid (Test 10)))))))))))
                                                    (via (Test 39))
                                                    (into
                                                     (Sub
                                                      (from
                                                       (Return
                                                        (value
                                                         (Mapn
                                                          (inputs
                                                           ((Named (uid (Test 39)))))))))
                                                      (via (Test 41))
                                                      (into
                                                       (Sub
                                                        (from
                                                         (Return
                                                          (value
                                                           (Mapn
                                                            (inputs
                                                             ((Named (uid (Test 41)))
                                                              (Named (uid (Test 38)))))))))
                                                        (via (Test 43))
                                                        (into
                                                         (Sub
                                                          (from
                                                           (Return
                                                            (value
                                                             (Mapn
                                                              (inputs
                                                               ((Named
                                                                 (uid (Test 43)))))))))
                                                          (via (Test 45))
                                                          (into
                                                           (Return
                                                            (value
                                                             (Mapn
                                                              (inputs
                                                               ((Named
                                                                 (uid (Test 45)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        |}]
    ;;
  end)
;;
