open! Core
open! Import

module Types = struct
  module Down = Unit
  module Acc = Unit

  module Up = struct
    type t = Type_id_set.t

    let empty = Type_id_set.empty
    let combine = Type_id_set.union
    let empty_for_lazy = empty
  end
end

module F (Recurse : Fix_transform.Recurse with module Types := Types) = struct
  open Trampoline.Let_syntax

  let transform_v (type a) () () (value : a Value.t) =
    match value with
    | { value = Named _; id; here = _ } -> (), Type_id_set.singleton id, value
    | value -> Recurse.on_value () () `Skipping_over value
  ;;

  let transform_c (type a) () () (t : a Computation.t) =
    match t with
    | Sub { via; from = _; into = _; here = _ } ->
      let%bind (), free_vars, c = Recurse.on_computation () () `Skipping_over t in
      (* the values bound in a sub are no longer free, so remove them *)
      return ((), Type_id_set.remove free_vars via, c)
    | Monitor_free_variables { inner; free_vars = _; here } ->
      let%bind (), free_vars, inner = Recurse.on_computation () () `Directly_on inner in
      return ((), free_vars, Computation.Monitor_free_variables { inner; free_vars; here })
    | _ -> Recurse.on_computation () () `Skipping_over t
  ;;
end

open Fix_transform.Make (Types) (F)

let run c =
  let (), _free_vars, r = Trampoline.run (transform_c () () c) in
  r
;;
