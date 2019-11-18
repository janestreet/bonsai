open! Core_kernel
open Bonsai_web
open Bonsai.Infix

let component =
  Bonsai.Incremental.of_incr (Incr.Clock.watch_now Incr.clock)
  >>| Time_ns.to_string
  >>| Vdom.Node.text
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:()
    ~bind_to_element_with_id:"app"
    component
;;
