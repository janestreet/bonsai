open! Core
open! Import

type 'a t = 'a Incr.Var.t

let create x = Incr.Var.create x

let set ~(here : [%call_pos]) t v =
  Stabilization_tracker.mark_incremental_dirty ();
  if Incr.am_stabilizing ()
  then
    raise_s
      [%message
        "Bonsai.Var mutated during the computation of a Bonsai value"
          (here : Source_code_position.t)];
  Incr.Var.set t v
;;

let update t ~f =
  let old = Incr.Var.value t in
  set t (f old)
;;

let get t = Incr.Var.value t
let value ~(here : [%call_pos]) t = t |> Incr.Var.watch |> Value.of_incr ~here
let incr_var = Fn.id
