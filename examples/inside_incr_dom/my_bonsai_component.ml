open! Core
open Bonsai_web
open Bonsai.Let_syntax

include
  (val Bonsai_web.To_incr_dom.convert (fun (_ : unit Bonsai.Value.t) ->
     let%sub counters = Bonsai_web_counters_example.application in
     let%sub () =
       Bonsai.Edge.lifecycle
         ()
         ~on_activate:(Value.return (Bonsai.Effect.print_s [%message "hi!"]))
     in
     let%sub () =
       Bonsai.Clock.every
         ~when_to_start_next_effect:`Every_multiple_of_period_blocking
         (Time_ns.Span.of_sec 1.0)
         (Value.return (Bonsai.Effect.print_s [%message "tick"]))
     in
     let%sub wait_after_display = Bonsai.Edge.wait_after_display in
     let%sub print_button =
       let%arr wait_after_display = wait_after_display in
       Vdom.Node.button
         ~attrs:
           [ Vdom.Attr.on_click (fun _ ->
               let%bind.Effect () = wait_after_display in
               Effect.print_s [%message "after display"])
           ]
         [ Vdom.Node.text "Print after display" ]
     in
     let%arr counters = counters
     and print_button = print_button in
     Vdom.Node.div [ counters; print_button ]))
