open! Core
open Bonsai_web

let timetraveled_component =
  Bonsai.map
    (Spacetime.create Bonsai_web_counters_example.application)
    ~f:(fun (app, timetravel) -> timetravel app)
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~bind_to_element_with_id:"app"
    timetraveled_component
;;
