open! Core
open Bonsai
open Bonsai.Let_syntax

include
  (val Bonsai_web.To_incr_dom.convert (fun (_ : unit Bonsai.Value.t) ->
     let%sub counters = Bonsai_web_counters_example.application in
     let%sub () =
       Bonsai.Edge.lifecycle
         ()
         ~on_activate:(Value.return (Bonsai.Effect.print_s [%message "hi!"]))
     in
     return counters))
