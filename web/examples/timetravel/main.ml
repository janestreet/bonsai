open! Core_kernel
open Bonsai_web

let timetraveled_component =
  Bonsai.map
    (Spacetime.create Bonsai_web_counters_example.application_component)
    ~f:(fun (app, timetravel) -> timetravel app)
;;

let initial_model = Spacetime.Model.create Bonsai_web_counters_example.initial_model

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model
    ~bind_to_element_with_id:"app"
    timetraveled_component
;;
