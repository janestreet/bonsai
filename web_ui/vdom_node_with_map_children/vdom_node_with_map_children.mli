open! Core
open! Bonsai_web
open! Js_of_ocaml

(** When given a map of vdom nodes, this function will wrap them in an element whose
    tag is determined by the first argument, and efficiently diff them against new nodes
    in the future that were created by this function. *)
val make : tag:string -> ?attr:Vdom.Attr.t -> (_, Vdom.Node.t, _) Map.t -> Vdom.Node.t
