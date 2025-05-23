open! Core
open! Import
module Proc_incr = Proc_min.Proc_incr

let compute ?(here = Stdlib.Lexing.dummy_pos) t ~f =
  Proc_incr.compute_with_clock ~here t ~f:(fun _ input -> f input)
;;

let with_clock ?(here = Stdlib.Lexing.dummy_pos) f =
  Proc_incr.compute_with_clock ~here (Value.return ~here ()) ~f:(fun clock _ -> f clock)
;;

let to_value ?(here = Stdlib.Lexing.dummy_pos) incr =
  { Value.value = Incr incr
  ; here
  ; id = Type_equal.Id.create ~name:"to_value" sexp_of_opaque
  }
;;
