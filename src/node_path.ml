open! Core
open! Import

type t =
  { choices : int list
  ; depth : int
  }
[@@deriving compare, bin_io]

let to_string { choices; depth } =
  let buffer = Buffer.create 10 in
  (match choices with
   | [] -> ()
   | choice :: choices ->
     Buffer.add_string buffer (Int.to_string choice);
     List.iter choices ~f:(fun choice ->
       Buffer.add_char buffer '-';
       Buffer.add_string buffer (Int.to_string choice)));
  Buffer.add_char buffer '_';
  Buffer.add_string buffer (Int.to_string depth);
  Buffer.contents buffer
;;

let of_string string =
  match String.split ~on:'_' string with
  | [ choices; depth ] ->
    let choices =
      match choices with
      | "" -> []
      | choices -> String.split ~on:'-' choices |> List.map ~f:Int.of_string
    in
    let depth = Int.of_string depth in
    { choices; depth }
  | _ -> raise_s [%message [%here] "failed to deserialize node-path" (string : string)]
;;

let sexp_of_t t = Sexp.Atom (to_string t)
let t_of_sexp s = of_string (string_of_sexp s)
let empty = { choices = []; depth = 0 }
let choice_point t n = { choices = t.choices @ [ n ]; depth = 0 }
let descend t = { t with depth = t.depth + 1 }

include Comparable.Make_binable (struct
    type nonrec t = t [@@deriving compare, sexp, bin_io]
  end)

let%test_module _ =
  (module struct
    let test t =
      let s = to_string t in
      print_endline s;
      assert ([%compare.equal: t] t (t_of_sexp (sexp_of_t t)))
    ;;

    let%expect_test _ =
      test { choices = [ 1; 2; 2; 1; 0 ]; depth = 0 };
      [%expect {| 1-2-2-1-0_0 |}]
    ;;

    let%expect_test _ =
      test { choices = []; depth = 1 };
      [%expect {| _1 |}]
    ;;

    let%expect_test _ =
      test empty;
      [%expect {| _0 |}]
    ;;

    let%expect_test _ =
      test { choices = [ 30 ]; depth = 1 };
      [%expect {| 30_1 |}]
    ;;
  end)
;;
