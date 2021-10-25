open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

val component
  :  Header_tree.t Value.t
  -> set_column_width:(index:int -> [ `Px of float ] -> unit Vdom.Effect.t) Value.t
  -> set_header_height:(int -> unit Vdom.Effect.t) Value.t
  -> Vdom.Node.t Computation.t
