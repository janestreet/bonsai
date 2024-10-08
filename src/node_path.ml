open Core.Core_stable

module Stable = struct
  module V1 = struct
    include String.V1
  end
end

open Stable
open! Core
open! Import

type choice_with_count =
  { choice : int
  ; count : int
  }

let choice_to_string { choice; count } =
  match count with
  | 1 -> Int.to_string choice
  | count -> {%string|%{choice#Int}x%{count#Int}|}
;;

type builder =
  { choices : choice_with_count Reversed_list.t
  ; depth : int
  }

let to_string { choices; depth } =
  let buffer = Buffer.create 10 in
  (match Reversed_list.rev choices with
   | [] -> ()
   | choice :: choices ->
     Buffer.add_string buffer (choice_to_string choice);
     List.iter choices ~f:(fun choice ->
       Buffer.add_char buffer '-';
       Buffer.add_string buffer (choice_to_string choice)));
  Buffer.add_char buffer '_';
  Buffer.add_string buffer (Int.to_string depth);
  Buffer.contents buffer
;;

let empty = { choices = []; depth = 0 }

let choice_point t n =
  match t.choices with
  | curr :: tl when curr.choice = n ->
    let choices = Reversed_list.({ curr with count = curr.count + 1 } :: tl) in
    { t with choices }
  | _ -> { choices = { choice = n; count = 1 } :: t.choices; depth = 0 }
;;

let descend t = { t with depth = t.depth + 1 }

module%test _ = struct
  let test t =
    let s = to_string t in
    print_endline s
  ;;

  let%expect_test _ =
    test
      { choices =
          Reversed_list.of_list_rev
            [ { choice = 1; count = 1 }
            ; { choice = 2; count = 2 }
            ; { choice = 1; count = 1 }
            ; { choice = 0; count = 1 }
            ]
      ; depth = 0
      };
    [%expect {| 1-2x2-1-0_0 |}]
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
    test { choices = [ { choice = 30; count = 1 } ]; depth = 1 };
    [%expect {| 30_1 |}]
  ;;
end

let finalize builder = to_string builder

module T : sig
  type t = V1.t
  type comparator_witness = V1.comparator_witness

  include Sexpable.S with type t := t
  include Binable.S with type t := t

  include
    Comparable.S_binable
    with type t := t
     and type comparator_witness := comparator_witness

  include Stringable.S with type t := t
end = struct
  include String
end

include T
