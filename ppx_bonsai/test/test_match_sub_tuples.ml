open Arrow.Let_syntax

let basic : 'a Arrow.v -> ('a * 'a) Arrow.c =
  fun x ->
  match%sub x, x with
  | foo, bar ->
    let%arr foo and bar in
    foo, bar
;;

let many
  : int Arrow.v -> float Arrow.v -> string Arrow.v -> (int * float * string) Arrow.c
  =
  fun a b c ->
  match%sub a, b, c with
  | a, b, c ->
    let%arr a and b and c in
    a, b, c
;;

let many_with_explicit_matching
  :  int Arrow.v -> float Arrow.v -> string Arrow.v -> bool Arrow.v
  -> (int * float * string * bool) Arrow.c
  =
  fun a b c d ->
  match%sub a, b, c, d with
  | a, b, c, true ->
    let%arr a and b and c in
    a, b, c, true
  | rem ->
    (* This test case shows that [rem] works. *)
    return rem
;;
