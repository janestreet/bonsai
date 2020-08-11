open! Core_kernel
open Bonsai_web
module Incr = Bonsai_web.Incr

let ( >>| ) = Bonsai.Arrow.Infix.( >>| )

let component =
  Bonsai.Arrow.With_incr.of_incr (Incr.Clock.watch_now Incr.clock)
  >>| Time_ns.to_string
  >>| Vdom.Node.text
;;

let (_ : _ Bonsai_web.Arrow.Start.Handle.t) =
  Bonsai_web.Arrow.Start.start_standalone
    ~initial_input:()
    ~bind_to_element_with_id:"app"
    component
;;
