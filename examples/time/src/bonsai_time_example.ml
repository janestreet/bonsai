open! Core
open Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=untestable-clock-component *)
let _untestable_component =
  let now = Incr.Clock.watch_now Incr.clock |> Bonsai.Incr.to_value in
  return (now >>| Time_ns.to_string_utc >>| Vdom.Node.text)
;;

(* $MDX part-end *)

(* $MDX part-begin=testable-clock-component *)
let component =
  let%sub now = Bonsai.Incr.with_clock Incr.Clock.watch_now in
  return (now >>| Time_ns.to_string_utc >>| Vdom.Node.text)
;;

(* $MDX part-end *)
