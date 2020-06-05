open Bonsai_web

let component = Bonsai.const (Vdom.Node.text "hello world")

let (_ : _ Start.Handle.t) =
  Start.start_standalone ~initial_input:() ~bind_to_element_with_id:"app" component
;;
