open! Core
open! Bonsai_web.Cont

(** Given a color-list map, this component will display the given rows
    using four approaches:

    1. Traditional [Vdom.Node.div (Map.data values)]
    2. Using Node_with_map_children
    3. Traditional (but with elements that have ~key properties on them)
    4. Node_with_map_children (but with elements that have ~key properties on them) *)
val view
  :  tag:string Bonsai.t
  -> attr:Vdom.Attr.t Bonsai.t
  -> Color_list.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
