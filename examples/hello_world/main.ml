open! Core
open! Bonsai_web

let component = Bonsai.const (Vdom.Node.text "hello world")

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
