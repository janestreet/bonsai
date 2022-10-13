open! Core
open! Bonsai_web

(** Given a color-list map, this component will display the given rows
    using four approaches:

    1. Traditional [Vdom.Node.div (Map.data values)]
    2. Using Node_with_map_children
    3. Traditional (but with elements that have ~key properties on them)
    4. Node_with_map_children (but with elements that have ~key properties on them) *)
val view : Color_list.t Value.t -> Vdom.Node.t Computation.t
