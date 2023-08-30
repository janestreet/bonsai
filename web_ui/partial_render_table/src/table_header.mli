open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

val component
  :  Header_tree.t Value.t
  -> column_widths:Column_size.t Int.Map.t Value.t
  -> set_column_width:(index:int -> [ `Px_float of float ] -> unit Vdom.Effect.t) Value.t
  -> set_header_client_rect:
       (Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.t -> unit Vdom.Effect.t)
       Value.t
  -> Vdom.Node.t Computation.t
