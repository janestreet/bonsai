open! Core
open! Bonsai_web
open! Js_of_ocaml


(** When given a map of vdom nodes, this function will wrap them in a div, and
    efficiently diff them against new nodes in the future that were created by
    this function. *)
val make : (_, Vdom.Node.t, _) Map.t -> Vdom.Node.t
