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
      (via 10)
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via 12)
          (into (
            Switch
            (match_ (Mapn (inputs ((Named (uid 12))))))
            (arms (
              (Return (value (Mapn (inputs (Incr)))))
              (Return (value (Mapn (inputs (Incr)))))))))))
        (via 15)
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 10))
                      (Named (uid 15))))))))))))))
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
        (via 27)
        (into (
          Switch
          (match_ (Mapn (inputs ((Named (uid 27))))))
          (arms (
            (Return (value (Mapn (inputs (Incr)))))
            (Return (value (Mapn (inputs (Incr)))))))))))
      (via 30)
      (into (
        Sub
        (from (Return (value (Mapn (inputs (Incr))))))
        (via 31)
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 30))
                      (Named (uid 31))))))))))))))
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
      (via 42)
      (into (
        Sub
        (from (Return (value (Mapn (inputs (Incr))))))
        (via 43)
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 42))
                      (Named (uid 43))))))))))))))
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
        (via 53)
        (into (
          Return (
            value (
              Mapn (
                inputs ((Mapn (inputs ((Named (uid 53)) (Mapn (inputs (Incr))))))))))))))
      (via 56)
      (into (
        Sub
        (from (
          Sub
          (from (Return (value Incr)))
          (via 53)
          (into (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (inputs ((Named (uid 53)) (Mapn (inputs (Incr))))))))))))))
        (via 57)
        (into (
          Return (
            value (
              Mapn (
                inputs ((
                  Mapn (
                    inputs (
                      (Named (uid 56))
                      (Named (uid 57))))))))))))))
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
      (via 67)
      (into (
        Sub
        (from (
          Return (
            value (
              Mapn (
                inputs ((Mapn (inputs ((Named (uid 67)) (Mapn (inputs (Incr))))))))))))
        (via 70)
        (into (
          Sub
          (from (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (inputs ((Named (uid 67)) (Mapn (inputs (Incr))))))))))))
          (via 71)
          (into (
            Return (
              value (
                Mapn (
                  inputs ((
                    Mapn (
                      inputs (
                        (Named (uid 70))
                        (Named (uid 71))))))))))))))))
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
