open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let complicated_transformation str_input int_input =
  let%arr str_input = str_input
  and int_input = int_input in
  let derived_value =
    String.fold str_input ~init:0 ~f:(fun acc c -> acc + Char.to_int c)
    |> fun x -> x * int_input mod 294
  in
  [%string
    "Input: %{int_input#Int}; Other Input: %{str_input#String}; Derived value: \
     %{derived_value#Int} "]
;;

(* $MDX part-begin=stale_closed_naive *)
let set_and_run_effect_naive (other_input : string Bonsai.t) graph =
  let count, set_state = Bonsai.state 0 graph in
  let computed = complicated_transformation other_input count in
  let set_and_alert =
    let%arr computed = computed
    and set_state = set_state in
    fun new_state ->
      let%bind.Effect () = set_state new_state in
      Effect.alert computed
  in
  let%arr count = count
  and set_and_alert = set_and_alert in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_and_alert (count + 1)) ]
        [ Vdom.Node.text "increment count" ]
    ]
;;

(* $MDX part-end *)

let () =
  Util.run
    (set_and_run_effect_naive (Bonsai.return "Other Input"))
    ~id:"stale_closed_naive"
;;

(* $MDX part-begin=stale_closed_peek *)
let set_and_run_effect_peek (other_input : string Bonsai.t) graph =
  let count, set_state = Bonsai.state 0 graph in
  let computed = complicated_transformation other_input count in
  let peek_computed = Bonsai.peek computed graph in
  let set_and_alert =
    let%arr peek_computed = peek_computed
    and set_state = set_state in
    fun new_state ->
      let%bind.Effect () = set_state new_state in
      match%bind.Effect peek_computed with
      | Active computed -> Effect.alert computed
      | Inactive -> Effect.Ignore
  in
  let%arr count = count
  and set_and_alert = set_and_alert in
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_and_alert (count + 1)) ]
        [ Vdom.Node.text "increment count" ]
    ]
;;

(* $MDX part-end *)

let () =
  Util.run (set_and_run_effect_peek (Bonsai.return "Other Input")) ~id:"stale_closed_peek"
;;
