open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let counter graph =
  let state =
    Tuple2.uncurry Bonsai.both
    @@ Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
  in
  let%arr current_value, set_value = state in
  Vdom.Node.div
    [ Vdom.Node.textf "%d" current_value
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_value (current_value + 1)) ]
        [ Vdom.Node.text "increment" ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_value (current_value - 1)) ]
        [ Vdom.Node.text "decrement" ]
    ]
;;

let two_counters graph =
  let counter_1 = counter graph in
  let counter_2 = counter graph in
  let%arr counter_1 = counter_1
  and counter_2 = counter_2 in
  Vdom.Node.div [ counter_1; counter_2 ]
;;

(* Note: because neither component that comprises [two_counters] depends on one another,
   it could instead be written using computation's let-syntax, like so *)
let _two_counters__computation_map_style graph =
  let%map counter_1 = counter graph
  and counter_2 = counter graph in
  Vdom.Node.div [ counter_1; counter_2 ]
;;

let () = Bonsai_web.Start.start two_counters
