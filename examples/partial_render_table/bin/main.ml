open! Core
open! Bonsai_web
module PRT_example = Bonsai_partial_render_table_example

let () =
  let input = Value.return (PRT_example.Row.many_random 100_000) in
  let component = PRT_example.component input in
  Bonsai_web.Start.start ~bind_to_element_with_id:"app" component
;;
