open! Core
open! Import
open Bonsai_test
open Bonsai.Cont.Let_syntax

(* this test needs to be in its own file because the crash happens at runtime and will
   end the incremental universe *)
let%expect_test "store named in a ref" =
  let branch = Bonsai.Var.create false in
  let name_ref = ref None in
  let component graph =
    match%sub Bonsai.Var.value branch with
    | false ->
      let a, _ = Bonsai.Cont.state 5 graph in
      name_ref := Some a;
      let%map a = a
      and branch = Bonsai.Var.value branch in
      sprintf "%d %b" a branch
    | true ->
      (Bonsai.lazy_ [@alert "-deprecated"])
        (lazy
          (fun _graph ->
            let%map a = Option.value_exn !name_ref
            and branch = Bonsai.Var.value branch in
            sprintf "%d %b" a branch))
        graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| "5 false" |}];
  assert (Option.is_some !name_ref);
  Bonsai.Var.set branch true;
  Expect_test_helpers_core.require_does_raise [%here] (fun () -> Handle.show handle);
  [%expect
    {| "A Value.t introduced by some [let%sub] expression was used outside of the scope that it was declared in. Make sure that you aren't storing it inside a ref." |}]
;;
