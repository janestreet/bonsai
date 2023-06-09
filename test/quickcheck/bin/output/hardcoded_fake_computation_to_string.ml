open! Core
open Bonsai.For_open
open Bonsai.Let_syntax

let computation =
  match%sub Value.return (Either.Second 1) with
  | First x0 ->
    Bonsai.assoc
      (module Int)
      (Value.return (Map.of_alist_exn (module Int) [ 0, (0, 0); 1, (1, 1); 2, (2, 2) ]))
      ~f:(fun x1 x2 ->
        let%sub x3 = Bonsai.state 1 ~equal:Int.equal ~sexp_of_model:Int.sexp_of_t in
        return (Value.both (Value.map x2 ~f:(fun _ -> -1)) x3))
  | Second x4 ->
    Bonsai.assoc
      (module Int)
      (Value.return (Map.of_alist_exn (module Int) [ 0, (0, 0); 1, (1, 1); 2, (2, 2) ]))
      ~f:(fun x5 x6 ->
        let%sub x7 = Bonsai.state 1 ~equal:Int.equal ~sexp_of_model:Int.sexp_of_t in
        return (Value.both (Value.map x6 ~f:(fun _ -> -1)) x7))
[@@warning "-27"]
;;
