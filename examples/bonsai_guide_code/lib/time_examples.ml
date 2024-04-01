open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

(* $MDX part-begin=clock_now *)
let current_time graph =
  let%arr now = Bonsai.Clock.now graph in
  Vdom.Node.text (Time_ns.to_string_utc now)
;;

(* $MDX part-end *)

(* $MDX part-begin=clock_approx_now *)
let approx_current_time graph =
  let%arr now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph in
  Vdom.Node.text (Time_ns.to_string_utc now)
;;

(* $MDX part-end *)

let long_effect =
  Effect.of_deferred_thunk (fun () ->
    Async_kernel.after (Time_ns.Span.of_int_ms (Random.int 2000)))
;;

(* $MDX part-begin=current_time_effect *)

let measure_time graph =
  let%arr get_time = Bonsai.Clock.get_current_time graph in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          let%bind.Effect start = get_time in
          let%bind.Effect () = long_effect in
          let%bind.Effect end_ = get_time in
          let diff = Time_ns.diff end_ start |> Time_ns.Span.to_string_hum in
          Effect.alert [%string "that took: %{diff}"])
      ]
    [ Vdom.Node.text "Click to measure a long effect." ]
;;

(* $MDX part-end *)

(* $MDX part-begin=clock_sleep *)
let clock_sleep_demo graph =
  let%arr sleep = Bonsai.Clock.sleep graph in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          let%bind.Effect () = sleep (Time_ns.Span.of_sec 2.) in
          Effect.alert "... 2 seconds later...")
      ]
    [ Vdom.Node.text "delayed alert" ]
;;

(* $MDX part-end *)

(* $MDX part-begin=clock_every *)
let clock_every_demo graph =
  let count, set_count = Bonsai.state 0 graph in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:false
    (Time_ns.Span.of_sec 1.0)
    (let%arr count = count
     and set_count = set_count in
     set_count (count + 1))
    graph;
  let%arr count = count in
  Vdom.Node.text [%string "Seconds since you opened the page: %{count#Int}"]
;;

(* $MDX part-end *)
