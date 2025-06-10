open! Core
open! Import
module Proc_incr = Proc_min.Proc_incr

let compute ~(here : [%call_pos]) t ~f =
  Proc_incr.compute_with_clock ~here t ~f:(fun _ input -> f input)
;;

let with_clock ~(here : [%call_pos]) f =
  Proc_incr.compute_with_clock ~here (Value.return ~here ()) ~f:(fun clock _ -> f clock)
;;

let to_value ~(here : [%call_pos]) incr = { Value.value = Incr incr; here }
