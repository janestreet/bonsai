open! Core

(* We care about List.chunks_of never returning an empty list when it is given an
   non_empty list. This test just sanity check this. *)

let%expect_test {|List.chunks_of never returns an empty list when it is given a non-empty list|}
  =
  let%quick_test prop (non_empty_list : unit Nonempty_list.t) =
    let chunks = Nonempty_list.to_list non_empty_list |> List.chunks_of ~length:7 in
    let is_empty = List.is_empty chunks in
    if is_empty
    then
      Expect_test_helpers_base.print_cr
        [%message "Error! expected chunks to be non-empty"]
    else (
      print_string "chunks list is non-empty (a good thing!): ";
      print_endline (Bool.to_string (not is_empty));
      [%expect {| chunks list is non-empty (a good thing!): true |}])
  in
  ()
;;
