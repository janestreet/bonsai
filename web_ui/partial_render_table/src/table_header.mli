open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

val component
  :  themed_attrs:Table_view.Themed.t Value.t
  -> 'column_id Header_tree.t Value.t
  -> column_widths:('column_id, Column_size.t, 'column_id_cmp) Map.t Value.t
  -> set_column_width:
       (column_id:'column_id -> [ `Px_float of float ] -> unit Vdom.Effect.t) Value.t
  -> set_header_client_rect:
       (Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.t -> unit Vdom.Effect.t)
       Value.t
  -> Table_view.Header.t Computation.t
