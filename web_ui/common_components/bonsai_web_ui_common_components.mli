open! Core
open! Bonsai_web

module Pills : sig
  val of_list
    :  ?extra_container_attr:Vdom.Attr.t Value.t
    -> ?extra_pill_attr:Vdom.Attr.t Value.t
    -> to_string:('a -> string) Value.t
    -> inject_selected_options:('a list -> unit Effect.t) Value.t
    -> 'a list Value.t
    -> Vdom.Node.t Computation.t

  val of_set
    :  ?extra_container_attr:Vdom.Attr.t Value.t
    -> ?extra_pill_attr:Vdom.Attr.t Value.t
    -> to_string:('a -> string) Value.t
    -> inject_selected_options:(('a, 'cmp) Set.t -> unit Effect.t) Value.t
    -> ('a, 'cmp) Set.t Value.t
    -> Vdom.Node.t Computation.t
end
