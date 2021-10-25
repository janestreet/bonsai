open! Core
open! Bonsai_web
module PRT_example = Bonsai_partial_render_table_example

let (_ : _ Start.Handle.t) =
  let input = Value.return (PRT_example.Row.many_random 100_000) in
  let component = PRT_example.component input in
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
