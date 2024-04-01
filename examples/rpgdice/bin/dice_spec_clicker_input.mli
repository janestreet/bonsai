open! Core
open! Bonsai_web.Cont

val component
  :  Bonsai.graph
  -> (Bonsai_web_rpgdice_example.Roll_spec.t * Vdom.Node.t) Bonsai.t
