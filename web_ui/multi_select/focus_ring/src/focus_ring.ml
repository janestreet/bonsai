open! Core

type 'a t =
  { current_focus : 'a
  ; before_rev : 'a list
  ; after : 'a list
  }
[@@deriving fields ~getters, compare, equal, sexp]

let of_nonempty_list_exn full_list =
  { current_focus = List.hd_exn full_list
  ; before_rev = []
  ; after = List.tl_exn full_list
  }
;;

let next t =
  match t.after with
  | next :: rest ->
    { current_focus = next; before_rev = t.current_focus :: t.before_rev; after = rest }
  | [] ->
    let full_list = List.rev (t.current_focus :: t.before_rev) in
    { current_focus = List.hd_exn full_list
    ; before_rev = []
    ; after = List.tl_exn full_list
    }
;;

let prev t =
  match t.before_rev with
  | prev :: rest ->
    { current_focus = prev; before_rev = rest; after = t.current_focus :: t.after }
  | [] ->
    let full_list_rev = List.rev (t.current_focus :: t.after) in
    { current_focus = List.hd_exn full_list_rev
    ; before_rev = List.tl_exn full_list_rev
    ; after = []
    }
;;

let set t ~f =
  let full_list = List.rev_append t.before_rev (t.current_focus :: t.after) in
  let before, after = List.split_while full_list ~f:(fun a -> not (f a)) in
  match after with
  | [] -> None
  | focus :: after -> Some { current_focus = focus; before_rev = List.rev before; after }
;;

let%expect_test _ =
  let t = ref (of_nonempty_list_exn [ 1; 2; 3; 4 ]) in
  let move f =
    t := f !t;
    print_s [%sexp (!t : int t)]
  in
  print_s [%sexp (!t : int t)];
  [%expect {| ((current_focus 1) (before_rev ()) (after (2 3 4))) |}];
  move next;
  [%expect {| ((current_focus 2) (before_rev (1)) (after (3 4))) |}];
  move next;
  [%expect {| ((current_focus 3) (before_rev (2 1)) (after (4))) |}];
  move next;
  [%expect {| ((current_focus 4) (before_rev (3 2 1)) (after ())) |}];
  move next;
  [%expect {| ((current_focus 1) (before_rev ()) (after (2 3 4))) |}];
  move next;
  [%expect {| ((current_focus 2) (before_rev (1)) (after (3 4))) |}];
  move prev;
  [%expect {| ((current_focus 1) (before_rev ()) (after (2 3 4))) |}];
  move prev;
  [%expect {| ((current_focus 4) (before_rev (3 2 1)) (after ())) |}];
  move prev;
  [%expect {| ((current_focus 3) (before_rev (2 1)) (after (4))) |}];
  move (fun t -> Option.value_exn (set t ~f:(fun a -> Int.equal a 1)));
  [%expect {| ((current_focus 1) (before_rev ()) (after (2 3 4))) |}];
  move (fun t -> Option.value_exn (set t ~f:(fun a -> Int.equal a 3)));
  [%expect {| ((current_focus 3) (before_rev (2 1)) (after (4))) |}]
;;
