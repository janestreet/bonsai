open! Core
open! Import

type 'a t = 'a Incr.Var.t

let create x = Incr.Var.create x

let set t v =
  if Incr.am_stabilizing ()
  then failwith "Bonsai.Var mutated during the computation of a Bonsai value";
  Incr.Var.set t v
;;

let update t ~f =
  let old = Incr.Var.value t in
  set t (f old)
;;

let get t = Incr.Var.value t
let value t = t |> Incr.Var.watch |> Value.of_incr
