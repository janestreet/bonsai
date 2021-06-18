open! Core

include
  (val Bonsai_web.To_incr_dom.convert (fun (_ : unit Bonsai.Value.t) ->
     Bonsai_web_counters_example.application))
