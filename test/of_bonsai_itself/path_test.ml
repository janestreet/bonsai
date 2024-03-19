open! Core
open! Import
open Bonsai_test
open Bonsai.For_open
open Bonsai.Let_syntax
module Path = Bonsai.Private.Path

let%expect_test "path" =
  let component =
    let%sub () = opaque_const () in
    let%sub path = Bonsai.Private.path in
    return (Value.map path ~f:Path.to_unique_identifier_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is
     added by the testing helpers. *)
  [%expect {| bonsai_path |}]
;;

let%expect_test "path constant folding" =
  let component =
    let%sub () = Bonsai.const () in
    let%sub path = Bonsai.Private.path in
    return (Value.map path ~f:Path.to_unique_identifier_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is
     added by the testing helpers. *)
  [%expect {| bonsai_path |}]
;;

let assert_path_unique_id_is_alpha path =
  let unique_id = Path.to_unique_identifier_string path in
  assert (
    String.for_all unique_id ~f:(function
      | 'a' .. 'z' | '_' -> true
      | _ -> false))
;;

let%test_unit "all the values are alpha" =
  let string_id = Type_equal.Id.create ~name:"string" [%sexp_of: string] in
  let keyed = Path.Elem.keyed ~compare:String.compare string_id |> unstage in
  Quickcheck.test
    String.quickcheck_generator
    ~sexp_of:[%sexp_of: string]
    ~f:(fun string ->
    let path = Path.append Path.empty (Path.Elem.Assoc (keyed string)) in
    assert_path_unique_id_is_alpha path)
;;

let%test_unit "larger groupings of paths behave" =
  let string_id = Type_equal.Id.create ~name:"string" [%sexp_of: string] in
  let keyed = Path.Elem.keyed ~compare:String.compare string_id |> unstage in
  let module P = struct
    (* Make a dumb version of this module so that we can derive quickcheck for it. *)
    type t =
      | From
      | Into
      | Assoc of string
      | Switch of int
    [@@deriving quickcheck, sexp]

    let to_path_element = function
      | From -> Path.Elem.Subst_from
      | Into -> Path.Elem.Subst_into
      | Assoc s -> Path.Elem.Assoc (keyed s)
      | Switch i -> Path.Elem.Switch i
    ;;
  end
  in
  Quickcheck.test
    [%quickcheck.generator: P.t list]
    ~sexp_of:[%sexp_of: P.t list]
    ~f:(fun path ->
    let path =
      path |> List.map ~f:P.to_path_element |> List.fold ~init:Path.empty ~f:Path.append
    in
    assert_path_unique_id_is_alpha path)
;;

type simple_path = [ `Subst_into | `Subst_from | `Assoc of Int.t | `Switch of Int.t ] list
[@@deriving sexp, quickcheck]

let iterations = ref 0
let compare_true = ref 0
let compare_false = ref 0
let compare_true_empty_list = ref 0
let compare_false_empty_list = ref 0

let%quick_test ("Bisimulating run length encoding path id comparison and slow but \
                 simpler comparison" [@remember_failures])
  =
  fun ((a, b) : simple_path * simple_path) ->
  incr iterations;
  let int_id = Type_equal.Id.create ~name:"int" [%sexp_of: int] in
  let path_a, path_b =
    Tuple2.map (a, b) ~f:(fun elements ->
      List.fold elements ~init:Bonsai.Private.Path.empty ~f:(fun path element ->
        let element =
          match element with
          | `Subst_from -> Bonsai.Private.Path.Elem.Subst_from
          | `Subst_into -> Subst_into
          | `Assoc i -> Assoc (T { key = i; id = int_id; compare = [%compare: int] })
          | `Switch i -> Switch i
        in
        Bonsai.Private.Path.append path element))
  in
  let correct_result =
    Bonsai.Private.Path.For_testing.slow_but_correct_compare_for_bisimulation
      path_a
      path_b
  in
  let fast_result = Bonsai.Private.Path.compare path_a path_b in
  if correct_result = 0
  then (
    incr compare_true;
    match a, b with
    | [], [] -> incr compare_true_empty_list
    | _ -> ())
  else (
    incr compare_false;
    match a, b with
    | [], _ | _, [] -> incr compare_false_empty_list
    | _ -> ());
  assert (correct_result = fast_result)
;;

let%expect_test ("distribution of quick_test samples" [@tags "no-js"]) =
  print_s
    [%message
      ""
        ~total:(!iterations : int)
        ~the_same:(!compare_true - !compare_true_empty_list : int)
        ~not_the_same:(!compare_false - !compare_false_empty_list : int)
        ~includes_the_empty_list:
          (!compare_true_empty_list + !compare_false_empty_list : int)];
  [%expect
    {|
    ((total                   10000)
     (the_same                62)
     (not_the_same            6095)
     (includes_the_empty_list 3843))
    |}]
;;

let%quick_test ("Bisimulating run length encoding path id comparison and slow but same \
                 list" [@remember_failures])
  =
  fun (path : simple_path) ->
  let int_id = Type_equal.Id.create ~name:"int" [%sexp_of: int] in
  let path_a, path_b =
    Tuple2.map (path, path) ~f:(fun path ->
      (* Constructing the same path twice is silly, but it's so that the phys_equal
         doesn't accidentally prevent the functions we want to compare from running... *)
      List.fold path ~init:Bonsai.Private.Path.empty ~f:(fun path element ->
        let element =
          match element with
          | `Subst_from -> Bonsai.Private.Path.Elem.Subst_from
          | `Subst_into -> Subst_into
          | `Assoc i -> Assoc (T { key = i; id = int_id; compare = [%compare: int] })
          | `Switch i -> Switch i
        in
        Bonsai.Private.Path.append path element))
  in
  let correct_result =
    Bonsai.Private.Path.For_testing.slow_but_correct_compare_for_bisimulation
      path_a
      path_b
  in
  let fast_result = Bonsai.Private.Path.compare path_a path_b in
  assert (correct_result = fast_result)
;;
