open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=juxtapose_part_1 *)
let juxtapose_digits ~(delimiter : string) (a : int Value.t) (b : int Value.t)
  : string Computation.t
  =
  let%arr a = a
  and b = b in
  Int.to_string a ^ delimiter ^ Int.to_string b
;;

(* $MDX part-end *)

(* $MDX part-begin=juxtapose_part_2 *)
let _juxtapose_and_sum (a : int Value.t) (b : int Value.t) : string Computation.t =
  let%sub juxtaposed = juxtapose_digits ~delimiter:" + " a b in
  let%sub sum =
    let%arr a = a
    and b = b in
    Int.to_string (a + b)
  in
  let%arr juxtaposed = juxtaposed
  and sum = sum in
  juxtaposed ^ " = " ^ sum
;;

(* $MDX part-end *)

(* $MDX part-begin=problem_with_map_part_1 *)
let component (xs : int list Value.t) : string Computation.t =
  let sum =
    let%map xs = xs in
    List.fold xs ~init:0 ~f:( + )
  in
  let average =
    let%map sum = sum
    and xs = xs in
    let length = List.length xs in
    if length = 0 then 0 else sum / length
  in
  let%arr sum = sum
  and average = average in
  [%string "sum = %{sum#Int}, average = %{average#Int}"]
;;

(* $MDX part-end *)

let _ = component

(* $MDX part-begin=problem_with_map_part_2 *)
let component (xs : int list Value.t) : string Computation.t =
  let%sub sum =
    return
      (let%map xs = xs in
       List.fold xs ~init:0 ~f:( + ))
  in
  let%sub average =
    return
      (let%map sum = sum
       and xs = xs in
       let length = List.length xs in
       if length = 0 then 0 else sum / length)
  in
  return
    (let%map sum = sum
     and average = average in
     [%string "sum = %{sum#Int}, average = %{average#Int}"])
;;

(* $MDX part-end *)

let _ = component

(* $MDX part-begin=problem_with_map_part_3 *)
let component (xs : int list Value.t) : string Computation.t =
  let%sub sum =
    let%arr xs = xs in
    List.fold xs ~init:0 ~f:( + )
  in
  let%sub average =
    let%arr sum = sum
    and xs = xs in
    let length = List.length xs in
    if length = 0 then 0 else sum / length
  in
  let%arr sum = sum
  and average = average in
  [%string "sum = %{sum#Int}, average = %{average#Int}"]
;;

(* $MDX part-end *)

let _ = component

(* $MDX part-begin=counter_var *)
let counter_every_second : int Value.t =
  let counter_var : int Bonsai.Var.t = Bonsai.Var.create (-1) in
  every (Time_ns.Span.of_sec 1.0) (fun () ->
    Bonsai.Var.update counter_var ~f:(fun i -> i + 1));
  Bonsai.Var.value counter_var
;;

let view_for_counter : Vdom.Node.t Computation.t =
  let%arr counter = counter_every_second in
  Vdom.Node.textf "counter: %d" counter
;;

(* $MDX part-end *)

let () = Util.run view_for_counter ~id:"counter"

(* $MDX part-begin=counter_button *)
let (counter_button : Vdom.Node.t Computation.t) =
  let%sub count, set_count = Bonsai.state (module Int) ~default_model:0 in
  let%arr count = count
  and set_count = set_count in
  (* view-construction logic *)
  Vdom.Node.div
    [ Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attr:(Vdom.Attr.on_click (fun _ -> set_count (count + 1)))
        [ Vdom.Node.text "increment count" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run counter_button ~id:"counter_button"

(* $MDX part-begin=three_counters *)
let (three_counters : Vdom.Node.t Computation.t) =
  let%sub counter1 = counter_button in
  let%sub counter2 = counter_button in
  let%sub counter3 = counter_button in
  let%arr counter1 = counter1
  and counter2 = counter2
  and counter3 = counter3 in
  Vdom.Node.div [ counter1; counter2; counter3 ]
;;

(* $MDX part-end *)

let () = Util.run three_counters ~id:"three_counters"
