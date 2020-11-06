open! Core_kernel
open! Import
open Proc
open Bonsai.Let_syntax
module Path = Bonsai.Private.Path

let%expect_test "path" =
  let component =
    let%sub _ = Bonsai.const () in
    let%sub path = Bonsai.Private.path in
    return (Bonsai.Value.map path ~f:Path.to_unique_identifier_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is
     added by the testing helpers. *)
  [%expect {| bonsai_path_x_y_x |}]
;;

let%test_unit "all the values are alphanumeric" =
  let string_id = Type_equal.Id.create ~name:"string" [%sexp_of: string] in
  let keyed = Path.Elem.keyed ~compare:String.compare string_id |> unstage in
  Quickcheck.test
    String.quickcheck_generator
    ~sexp_of:[%sexp_of: string]
    ~f:(fun string ->
      let path = Path.append Path.empty (Path.Elem.Assoc (keyed string)) in
      let unique_id = Path.to_unique_identifier_string path in
      assert (
        String.for_all unique_id ~f:(function
          | 'a' .. 'z' | '_' -> true
          | _ -> false)))
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
      | Enum of string
    [@@deriving quickcheck, sexp]

    let to_path_element = function
      | From -> Path.Elem.Subst_from
      | Into -> Path.Elem.Subst_into
      | Assoc s -> Path.Elem.Assoc (keyed s)
      | Enum s -> Path.Elem.Enum (keyed s)
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
      let unique_id = Path.to_unique_identifier_string path in
      assert (
        String.for_all unique_id ~f:(function
          (* 'p' is 0b1111 + 'a' *)
          | '_' | 'a' .. 'z' -> true
          | _ -> false)))
;;
