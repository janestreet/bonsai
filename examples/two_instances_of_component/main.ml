open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let counter =
  let%sub state = Bonsai.state [%here] (module Int) ~default_model:0 in
  let%arr current_value, set_value = state in
  Vdom.Node.div
    [ Vdom.Node.textf "%d" current_value
    ; Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> set_value (current_value + 1)))
        [ Vdom.Node.text "increment" ]
    ; Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> set_value (current_value - 1)))
        [ Vdom.Node.text "decrement" ]
    ]
;;

let two_counters =
  let%sub counter_1 = counter in
  let%sub counter_2 = counter in
  let%arr counter_1 = counter_1
  and counter_2 = counter_2 in
  Vdom.Node.div [ counter_1; counter_2 ]
;;

(* Note: because neither component that comprises [two_counters] depends on one another,
   it could instead be written using computation's let-syntax, like so *)
let _two_counters__computation_map_style =
  let%map.Computation counter_1 = counter
  and counter_2 = counter in
  Vdom.Node.div [ counter_1; counter_2 ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" two_counters
;;
