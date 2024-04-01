open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* $MDX part-begin=counter *)
let counter graph : Vdom.Node.t Bonsai.t * int Bonsai.t =
  let count, set_count = Bonsai.state 0 graph in
  let view =
    let%arr count = count
    and set_count = set_count in
    (* view-construction logic *)
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count - 1)) ]
          [ Vdom.Node.text "-1" ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count + 1)) ]
          [ Vdom.Node.text "+1" ]
      ]
  in
  view, count
;;

(* $MDX part-end *)

let counter_ui graph =
  let view, _ = counter graph in
  view
;;

let () = Util.run counter_ui ~id:"counter_ui"

(* $MDX part-begin=two_counters_correct *)
let two_counters graph =
  let counter1, _count1 = counter graph in
  let counter2, _count2 = counter graph in
  let%arr counter1 = counter1
  and counter2 = counter2 in
  Vdom.Node.div [ counter1; counter2 ]
;;

(* $MDX part-end *)

let () = Util.run two_counters ~id:"two_counters_correct"

(* $MDX part-begin=two_counters_wrong_1 *)
let two_counters_wrong_1 graph =
  let counter, _count = counter graph in
  let%arr counter1 = counter
  and counter2 = counter in
  Vdom.Node.div [ counter1; counter2 ]
;;

(* $MDX part-end *)

let () = Util.run two_counters_wrong_1 ~id:"two_counters_wrong_1"

(* $MDX part-begin=two_counters_wrong_2 *)
let two_counters_wrong_2 graph =
  let counter, _count = counter graph in
  let%arr counter = counter in
  Vdom.Node.div [ counter; counter ]
;;

(* $MDX part-end *)

let () = Util.run two_counters_wrong_2 ~id:"two_counters_wrong_2"

(* $MDX part-begin=counter_state_machine *)

let counter_state_machine graph : Vdom.Node.t Bonsai.t * int Bonsai.t =
  let count, inject =
    Bonsai.state_machine0
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model action ->
        match action with
        | `Increment -> model + 1
        | `Decrement -> model - 1)
      graph
  in
  let view =
    let%arr count = count
    and inject = inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text "-1" ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text "+1" ]
      ]
  in
  view, count
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun graph -> counter_state_machine graph |> Tuple2.get1)
    ~id:"counter_state_machine"
;;

(* $MDX part-begin=counter_state_machine1 *)

let counter_state_machine1 ~(step : int Bonsai.t) graph =
  let count, inject =
    Bonsai.state_machine1
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input model action ->
        match input with
        | Bonsai.Computation_status.Inactive ->
          (* This state machine is inactive, so it can't access the current value of [input].
             Just keep the original model *)
          model
        | Active step ->
          (match action with
           | `Increment -> model + step
           | `Decrement -> model - step))
      step
      graph
  in
  let view =
    let%arr step = step
    and count = count
    and inject = inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text [%string "-%{step#Int}"] ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text [%string "+%{step#Int}"] ]
      ]
  in
  view, count
;;

(* $MDX part-end *)

(* $MDX part-begin=counter_state_machine_chained *)
let counter_state_machine_chained graph =
  let counter1, count1 = counter_state_machine1 ~step:(Bonsai.return 1) graph in
  let counter2, count2 = counter_state_machine1 ~step:count1 graph in
  let counter3, _ = counter_state_machine1 ~step:count2 graph in
  let%arr counter1 = counter1
  and counter2 = counter2
  and counter3 = counter3 in
  Vdom.Node.div [ counter1; counter2; counter3 ]
;;

(* $MDX part-end *)

let () = Util.run counter_state_machine_chained ~id:"counter_state_machine_chained"
let counter = counter_state_machine1
