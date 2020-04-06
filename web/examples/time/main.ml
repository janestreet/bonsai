open! Core_kernel
open Bonsai_web
open Bonsai.Infix

let component =
  Bonsai.With_incr.of_incr (Incr.Clock.watch_now Incr.clock)
  >>| Time_ns.to_string
  >>| Vdom.Node.text
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone ~initial_input:() ~bind_to_element_with_id:"app" component
;;
