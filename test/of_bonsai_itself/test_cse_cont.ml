open! Core
open! Import
open Bonsai_test

module Bonsai = struct
  include Bonsai.Cont
  module Private = Bonsai.Private
end

open Bonsai.Let_syntax

let dummy_value () =
  Bonsai.map (opaque_const_value ()) ~f:(fun () -> print_endline "computing!")
;;

let use_two_unit_values a b =
  let%arr () = a
  and () = b in
  ()
;;

let sexp_of_computation c =
  let module Private = Bonsai.Private in
  c
  |> Private.top_level_handle
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
  let component _graph =
    let dummy = dummy_value () in
    let%map () = dummy
    and () = dummy in
    ()
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
          Return (
            value (
              Mapn (
                inputs (
                  (Named (uid (Test 2)))
                  (Named (uid (Test 2)))))))))
        (via (Test 4))
        (into (Return (value (Mapn (inputs ((Named (uid (Test 4))))))))))))
    computing! |}]
;;

let%expect_test "double-use spanning match%sub with value previously computed" =
  let component _graph =
    let a = dummy_value () in
    let b =
      match%sub opaque_const_value true with
      | true -> a
      | false -> a
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
        (from (Return (value (Mapn (inputs (Incr))))))
        (via (Test 5))
        (into (
          Sub
          (from (
            Switch
            (match_ (Named (uid (Test 5))))
            (arms (
              (Return (value (Named (uid (Test 2)))))
              (Return (value (Named (uid (Test 2)))))))))
          (via (Test 6))
          (into (
            Sub
            (from (
              Return (
                value (
                  Mapn (
                    inputs (
                      (Named (uid (Test 2)))
                      (Named (uid (Test 6)))))))))
            (via (Test 8))
            (into (Return (value (Mapn (inputs ((Named (uid (Test 8))))))))))))))))
    computing! |}]
;;

let%expect_test "double-use with first use inside scope" =
  let component _graph =
    let dummy_value = dummy_value () in
    let a =
      match%sub opaque_const_value true with
      | true -> dummy_value
      | false -> dummy_value
    in
    let b = dummy_value in
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
        (via (Test 5))
        (into (
          Sub
          (from (
            Switch
            (match_ (Named (uid (Test 5))))
            (arms (
              (Return (value (Named (uid (Test 2)))))
              (Return (value (Named (uid (Test 2)))))))))
          (via (Test 6))
          (into (
            Sub
            (from (
              Return (
                value (
                  Mapn (
                    inputs (
                      (Named (uid (Test 6)))
                      (Named (uid (Test 2)))))))))
            (via (Test 8))
            (into (Return (value (Mapn (inputs ((Named (uid (Test 8))))))))))))))))
    computing! |}]
;;

let%expect_test "double-use inside of some subs" =
  let component graph =
    let v = dummy_value () in
    let subcomponent _graph = v in
    let a = subcomponent graph in
    let b = subcomponent graph in
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
          Return (
            value (
              Mapn (
                inputs (
                  (Named (uid (Test 2)))
                  (Named (uid (Test 2)))))))))
        (via (Test 4))
        (into (Return (value (Mapn (inputs ((Named (uid (Test 4))))))))))))
    computing! |}]
;;

let%expect_test "double-use inside of some nested subs" =
  let component graph =
    let dummy_value = dummy_value () in
    let subcomponent graph =
      let a = opaque_const () graph in
      let%map () = a
      and () = dummy_value in
      ()
    in
    let a = subcomponent graph in
    let b = subcomponent graph in
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
        (from (Return (value (Mapn (inputs (Incr (Named (uid (Test 2)))))))))
        (via (Test 5))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
          (via (Test 7))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Incr (Named (uid (Test 2)))))))))
            (via (Test 10))
            (into (
              Sub
              (from (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))
              (via (Test 12))
              (into (
                Sub
                (from (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 7)))
                          (Named (uid (Test 12)))))))))
                (via (Test 14))
                (into (Return (value (Mapn (inputs ((Named (uid (Test 14))))))))))))))))))))
    computing! |}]
;;

let%expect_test "double-use inside supercomponent" =
  let component graph =
    let i = opaque_const () graph in
    let dummy_value = dummy_value () in
    let subcomponent _graph =
      let%map () = i
      and () = dummy_value in
      print_endline "more computing"
    in
    let a = subcomponent graph in
    let b = subcomponent graph in
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
        (from (Return (value (Mapn (inputs (Incr (Named (uid (Test 2)))))))))
        (via (Test 5))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
          (via (Test 7))
          (into (
            Sub
            (from (Return (value (Mapn (inputs (Incr (Named (uid (Test 2)))))))))
            (via (Test 9))
            (into (
              Sub
              (from (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))
              (via (Test 11))
              (into (
                Sub
                (from (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 7)))
                          (Named (uid (Test 11)))))))))
                (via (Test 13))
                (into (Return (value (Mapn (inputs ((Named (uid (Test 13))))))))))))))))))))
    computing!
    more computing
    more computing |}]
;;

let%expect_test "double-use with first use inside scope" =
  let component graph =
    let subcomponent graph =
      let path = Bonsai.path_id graph in
      let%map path = path in
      print_endline ("computing " ^ path);
      path
    in
    let a = subcomponent graph in
    let b = subcomponent graph in
    let%map a = a
    and b = b in
    a ^ " " ^ b
  in
  let handle =
    Handle.create ~optimize:false (Result_spec.string (module String)) component
  in
  [%expect {|
    computing bonsai_path_x_y_y_x_x
    computing bonsai_path_x_x_x |}];
  Handle.show handle;
  [%expect {| bonsai_path_x_x_x bonsai_path_x_y_y_x_x |}]
;;
