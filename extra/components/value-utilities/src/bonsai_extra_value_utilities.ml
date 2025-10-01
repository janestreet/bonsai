open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

let value_with_override ?sexp_of_model ?equal value (local_ graph) =
  let state, set_state = Bonsai.state_opt graph ?sexp_of_model ?equal in
  let value =
    match%sub state with
    | Some override -> override
    | None -> value
  in
  let setter =
    let%arr set_state in
    fun v -> set_state (Some v)
  in
  value, setter
;;

let dynamic_cutoff a ~equal (local_ graph) =
  Bonsai.both a equal
  |> Bonsai.cutoff ~equal:(fun (a, _old_equal) (b, equal) -> equal a b)
  |> Bonsai.arr1 graph ~f:Tuple2.get1
;;
