open! Core
open Bonsai_web_ui_element_size_hooks.Visibility_tracker
open Bonsai_web_ui_element_size_hooks.Visibility_tracker.For_testing

let%test_module "intersect and reproject" =
  (module struct
    let test ~child ~parent =
      intersect_and_reproject child parent |> [%sexp_of: int Bbox.t option] |> print_s
    ;;

    let%expect_test "full intersection" =
      test
        ~child:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parent:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 };
      [%expect {| (((min_x 0) (max_x 5) (min_y 0) (max_y 5))) |}]
    ;;

    let%expect_test "offset full intersection" =
      test
        ~child:{ min_x = 5.0; max_x = 10.0; min_y = 5.0; max_y = 10.0 }
        ~parent:{ min_x = 5.0; max_x = 10.0; min_y = 5.0; max_y = 10.0 };
      [%expect {| (((min_x 0) (max_x 5) (min_y 0) (max_y 5))) |}]
    ;;

    let%expect_test "no intersection x" =
      test
        ~child:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parent:{ min_x = 5.0; max_x = 10.0; min_y = 0.0; max_y = 5.0 };
      [%expect {| () |}]
    ;;

    let%expect_test "no intersection y" =
      test
        ~child:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parent:{ min_x = 0.0; max_x = 5.0; min_y = 5.0; max_y = 10.0 };
      [%expect {| () |}]
    ;;

    let%expect_test "no intersection corner" =
      test
        ~child:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parent:{ min_x = 5.0; max_x = 10.0; min_y = 5.0; max_y = 10.0 };
      [%expect {| () |}]
    ;;

    let%expect_test "scrolling" =
      test
        ~child:{ min_x = 0.0; max_x = 20.0; min_y = -100.0; max_y = 100.0 }
        ~parent:{ min_x = 0.0; max_x = 20.0; min_y = 0.0; max_y = 50.0 };
      [%expect {| (((min_x 0) (max_x 20) (min_y 100) (max_y 150))) |}]
    ;;
  end)
;;

let%test_module "intersect and reproject" =
  (module struct
    let test ~target ~parents ~window =
      compute_visibility ~client_bbox:target ~parents ~window_bbox:window
      |> [%sexp_of: int Bbox.t option]
      |> print_s
    ;;

    let%expect_test "full intersection, no parents" =
      test
        ~target:{ min_x = 1.0; max_x = 5.0; min_y = 1.0; max_y = 5.0 }
        ~window:{ min_x = 0.0; max_x = 6.0; min_y = 0.0; max_y = 6.0 }
        ~parents:[];
      [%expect {| (((min_x 0) (max_x 4) (min_y 0) (max_y 4))) |}]
    ;;

    let%expect_test "perfect intersection, no parents" =
      test
        ~target:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~window:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parents:[];
      [%expect {| (((min_x 0) (max_x 5) (min_y 0) (max_y 5))) |}]
    ;;

    let%expect_test "full intersection, single big parent" =
      test
        ~target:{ min_x = 2.0; max_x = 5.0; min_y = 2.0; max_y = 5.0 }
        ~window:{ min_x = 0.0; max_x = 7.0; min_y = 0.0; max_y = 7.0 }
        ~parents:[ { min_x = 1.0; max_x = 6.0; min_y = 1.0; max_y = 6.0 } ];
      [%expect {| (((min_x 0) (max_x 3) (min_y 0) (max_y 3))) |}]
    ;;

    let%expect_test "partial intersection with window, no parent" =
      test
        ~target:{ min_x = -2.0; max_x = 3.0; min_y = -2.0; max_y = 3.0 }
        ~window:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parents:[];
      [%expect {| (((min_x 2) (max_x 5) (min_y 2) (max_y 5))) |}]
    ;;

    let%expect_test "partial intersection with window, single perfect-intersecting parent"
      =
      test
        ~target:{ min_x = -2.0; max_x = 3.0; min_y = -2.0; max_y = 3.0 }
        ~window:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parents:[ { min_x = -2.0; max_x = 3.0; min_y = -2.0; max_y = 3.0 } ];
      [%expect {| (((min_x 2) (max_x 5) (min_y 2) (max_y 5))) |}]
    ;;

    let%expect_test "partial intersection with window, partially-intersecting parent" =
      test
        ~target:{ min_x = -2.0; max_x = 3.0; min_y = -2.0; max_y = 3.0 }
        ~window:{ min_x = 0.0; max_x = 5.0; min_y = 0.0; max_y = 5.0 }
        ~parents:[ { min_x = 1.0; max_x = 3.0; min_y = 1.0; max_y = 3.0 } ];
      [%expect {| (((min_x 3) (max_x 5) (min_y 3) (max_y 5))) |}]
    ;;
  end)
;;
