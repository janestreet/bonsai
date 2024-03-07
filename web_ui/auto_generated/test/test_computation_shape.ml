open! Core
open! Bonsai.Let_syntax
open! Import
open! Bonsai_web_test
module Private = Bonsai.Private
module Auto_generated = Bonsai_web_ui_auto_generated

let count_computation_nodes name c =
  let skeleton =
    Private.Skeleton.Computation.of_computation (Private.top_level_handle c)
  in
  let o =
    object
      inherit [int] Private.Skeleton.Traverse.fold as super
      method! computation c acc = super#computation c (acc + 1)
    end
  in
  let count = o#computation skeleton 0 in
  print_endline (sprintf "%s: %d nodes" name count);
  count
;;

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

let test_form (type a) (module M : S with type t = a) (ts : a list) =
  let run ~optimize c =
    let handle = Handle.create ~optimize (form_result_spec M.sexp_of_t) c in
    List.iter ts ~f:(fun t ->
      Handle.do_actions handle [ t ];
      Handle.recompute_view handle)
  in
  let computation = Auto_generated.form (module M) () in
  let no_opt_count =
    let raw_computation = Private.top_level_handle computation in
    run (fun graph -> Private.perform graph raw_computation) ~optimize:false;
    count_computation_nodes "no optimization" (fun graph ->
      Private.perform graph raw_computation)
  in
  let with_opt_count =
    let raw_computation = Private.top_level_handle computation |> Private.pre_process in
    run (fun graph -> Private.perform graph raw_computation) ~optimize:true;
    count_computation_nodes "with optimization" (fun graph ->
      Private.perform graph raw_computation)
  in
  print_endline
    (sprintf
       "reduced to %.1f%%"
       (100.0 *. (Int.to_float with_opt_count /. Int.to_float no_opt_count)));
  ()
;;

let%expect_test "int" =
  test_form (module Int) [ 5 ];
  [%expect
    {|
    no optimization: 443 nodes
    with optimization: 31 nodes
    reduced to 7.0%
  |}]
;;

let%expect_test "option>variant>record form" =
  let module T = struct
    type record =
      { a : int
      ; b : string
      }
    [@@deriving sexp, sexp_grammar]

    type variant =
      | A
      | B of record
    [@@deriving sexp, sexp_grammar]

    type t = variant option [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ None; Some A; Some (B { a = 5; b = "hello" }) ];
  [%expect
    {|
    no optimization: 3697 nodes
    with optimization: 225 nodes
    reduced to 6.1%
    |}]
;;

let%expect_test "variant form" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ A; B 5 ];
  [%expect
    {|
    no optimization: 1569 nodes
    with optimization: 109 nodes
    reduced to 6.9% |}]
;;

let%expect_test "record form" =
  let module T = struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ { a = 5; b = "hello" } ];
  [%expect
    {|
    no optimization: 1697 nodes
    with optimization: 105 nodes
    reduced to 6.2% |}]
;;

let%expect_test "option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ None; Some 5 ];
  [%expect
    {|
    no optimization: 1317 nodes
    with optimization: 73 nodes
    reduced to 5.5% |}]
;;
