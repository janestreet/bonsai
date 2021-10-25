open! Core
open! Bonsai_web
module Row = Row

val component
  :  ?filter:(key:string -> data:Row.t -> bool) Bonsai.Value.t
  -> Row.t String.Map.t Value.t
  -> Vdom.Node.t Computation.t
