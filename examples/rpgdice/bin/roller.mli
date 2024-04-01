open! Core
open! Bonsai_web.Cont

val component
  :  Bonsai_web_rpgdice_example.Roll_spec.t Or_error.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
