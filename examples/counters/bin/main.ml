open Bonsai_web
open Bonsai_web_counters_example

let (_ : _ Start.Proc.Handle.t) =
  Start.Proc.start
    Start.Proc.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    application
;;
