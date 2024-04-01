open! Core
open Bonsai_web.Cont
open Bonsai.Let_syntax

(* $MDX part-begin=untestable-clock-component *)
let _untestable_component =
  let now = Incr.Clock.watch_now Incr.clock |> Bonsai.Incr.to_value in
  return (now >>| Time_ns.to_string_utc >>| Vdom.Node.text)
;;

(* $MDX part-end *)

(* $MDX part-begin=testable-clock-component *)
let component graph =
  let now = Bonsai.Incr.with_clock ~f:Bonsai.Time_source.watch_now graph in
  now >>| Time_ns.to_string_utc >>| Vdom.Node.text
;;

(* $MDX part-end *)
