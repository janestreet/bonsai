open! Core
open! Import

(** [component] returns the search bar itself (as a Vdom.Node.t) as well as all matching
    icons. *)
val component : (Feather_icon.t list * Vdom.Node.t) Computation.t
