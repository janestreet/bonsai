open! Core
open! Import
open Bonsai.Let_syntax
open Proc

let dummy_value =
  Bonsai.Value.map (opaque_const_value ()) ~f:(fun () -> print_endline "computing!")
;;

let use_two_unit_values a b =
  let%arr () = a
  and () = b in
  ()
;;

let sexp_of_computation c =
  let module Private = Bonsai.Private in
  c
  |> Private.reveal_computation
  |> Private.Skeleton.Computation.of_computation
  |> Private.Skeleton.Computation.sanitize_for_testing
  |> Private.Skeleton.Computation.minimal_sexp_of_t
;;

let print_computation c = print_s (sexp_of_computation c)

let evaluate component =
  print_computation component;
  let _handle =
    Handle.create ~optimize:true (Result_spec.string (module Unit)) component
  in
  ()
;;

let%expect_test "double-use of a Value.t" =
  let component = use_two_unit_values dummy_value dummy_value in
  evaluate component;
  [%expect
    {|
    (Return (
      value (
        Mapn (
          inputs ((
            Mapn (
              inputs (
                (Mapn (inputs (Incr)))
                (Mapn (inputs (Incr)))))))))))
    computing!
    computing! |}]
;;

let%expect_test "double-use spanning match%sub with value previously computed" =
  let component =
    let%sub a = return dummy_value in
    let%sub b =
      match%sub opaque_const_value true with
      | true -> return dummy_value
      | false -> return dummy_value
    in
    use_two_unit_values a b
  in
  evaluate component;
  [%expect
    {|
    (Sub
      (from (Return (value (Mapn (inputs (Incr))))))
      (via (Test 2))
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via (Test 4))
          (into (
            Switch
            (match_ (Mapn (inputs ((Named (uid (Test 4)))))))
            (arms (
              (Return (value (Mapn (inputs (Incr)))))
              (Return (value (Mapn (inputs (Incr)))))))))))
        (via (Test 6))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 2)))
                      (Named (uid (Test 6)))))))))))))))
    computing!
    computing! |}]
;;

let%expect_test "double-use with first use inside scope" =
  let component =
    let%sub a =
      match%sub opaque_const_value true with
      | true -> return dummy_value
      | false -> return dummy_value
    in
    let%sub b = return dummy_value in
    use_two_unit_values a b
  in
  evaluate component;
  [%expect
    {|
    (Sub
      (from (
        Sub
        (from (Return (value Incr)))
        (via (Test 1))
        (into (
          Switch
          (match_ (Mapn (inputs ((Named (uid (Test 1)))))))
          (arms (
            (Return (value (Mapn (inputs (Incr)))))
            (Return (value (Mapn (inputs (Incr)))))))))))
      (via (Test 5))
      (into (
        Sub
        (from (Return (value (Mapn (inputs (Incr))))))
        (via (Test 6))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 5)))
                      (Named (uid (Test 6)))))))))))))))
    computing!
    computing! |}]
;;

let%expect_test "double-use inside of some subs" =
  let component =
    let subcomponent = return dummy_value in
    let%sub a = subcomponent in
    let%sub b = subcomponent in
    use_two_unit_values a b
  in
  evaluate component;
  [%expect
    {|
    (Sub
      (from (Return (value (Mapn (inputs (Incr))))))
      (via (Test 2))
      (into (
        Sub
        (from (Return (value (Mapn (inputs (Incr))))))
        (via (Test 3))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 2)))
                      (Named (uid (Test 3)))))))))))))))
    computing!
    computing! |}]
;;

let%expect_test "double-use inside of some nested subs" =
  let component =
    let subcomponent =
      let%sub a = opaque_const () in
      let%arr () = a
      and () = dummy_value in
      ()
    in
    let%sub a = subcomponent in
    let%sub b = subcomponent in
    use_two_unit_values a b
  in
  evaluate component;
  [%expect
    {|
    (Sub
      (from (
        Sub
        (from (Return (value Incr)))
        (via (Test 1))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (inputs ((Named (uid (Test 1))) (Mapn (inputs (Incr))))))))))))))
      (via (Test 6))
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via (Test 1))
          (into (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (inputs ((Named (uid (Test 1))) (Mapn (inputs (Incr))))))))))))))
        (via (Test 7))
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid (Test 6)))
                      (Named (uid (Test 7)))))))))))))))
    computing!
    computing! |}]
;;

let%expect_test "double-use inside supercomponent" =
  let component =
    let%sub i = opaque_const () in
    let subcomponent =
      let%arr () = i
      and () = dummy_value in
      print_endline "more computing"
    in
    let%sub a = subcomponent in
    let%sub b = subcomponent in
    use_two_unit_values a b
  in
  evaluate component;
  [%expect
    {|
    (Sub
      (from (Return (value Incr)))
      (via (Test 1))
      (into (
        Sub
        (from (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (inputs ((Named (uid (Test 1))) (Mapn (inputs (Incr))))))))))))
        (via (Test 6))
        (into (
          Sub
          (from (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (inputs ((Named (uid (Test 1))) (Mapn (inputs (Incr))))))))))))
          (via (Test 7))
          (into (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (
                      inputs (
                        (Named (uid (Test 6)))
                        (Named (uid (Test 7)))))))))))))))))
    computing!
    computing!
    more computing
    more computing |}]
;;

let%expect_test "double-use with first use inside scope" =
  let component =
    let subcomponent =
      let%sub path = Bonsai.path_id in
      let%arr path = path in
      print_endline ("computing " ^ path);
      path
    in
    let%sub a = subcomponent in
    let%sub b = subcomponent in
    let%arr a = a
    and b = b in
    a ^ " " ^ b
  in
  let handle =
    Handle.create ~optimize:false (Result_spec.string (module String)) component
  in
  [%expect {|
    computing bonsai_path_x_y_x_x_x
    computing bonsai_path_x_x_x_x |}];
  Handle.show handle;
  [%expect {| bonsai_path_x_x_x_x bonsai_path_x_y_x_x_x |}]
;;
