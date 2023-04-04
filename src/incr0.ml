open! Core
open! Import
module Proc_incr = Proc_min.Proc_incr

let compute t ~f = Proc_incr.compute_with_clock t ~f:(fun _ input -> f input)

let with_clock f =
  Proc_incr.compute_with_clock (Value.return ()) ~f:(fun clock _ -> f clock)
;;

let to_value incr =
  { Value.value = Value.Incr incr
  ; here = None
  ; id = Type_equal.Id.create ~name:"to_value" sexp_of_opaque
  }
;;
