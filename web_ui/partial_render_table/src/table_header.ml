open! Core
open! Bonsai_web
open! Js_of_ocaml
open! Incr_map_collate
open! Bonsai.Let_syntax


let attr_colspan i =
  match i with
  | 0 -> Vdom.Attr.style (Css_gen.display `None)
  | 1 -> Vdom.Attr.empty
  | i -> Vdom.Attr.create_float "colspan" (Int.to_float i)
;;

module Acc = struct
  type t =
    { level_map : Vdom.Node.t list Int.Map.t
    ; leaf_index : int
    }

  let empty = { level_map = Int.Map.empty; leaf_index = 0 }

  let visit_leaf { level_map; leaf_index } ~level ~node =
    let idx = leaf_index in
    let leaf_index = leaf_index + 1 in
    let level_map = Map.add_multi (level_map : _ Int.Map.t) ~key:level ~data:(node idx) in
    { leaf_index; level_map }
  ;;

  let visit_non_leaf { level_map; leaf_index } ~level ~node =
    let level_map = Map.add_multi (level_map : _ Int.Map.t) ~key:level ~data:node in
    { leaf_index; level_map }
  ;;

  let finalize { level_map; leaf_index = _ } =
    level_map |> Map.data |> List.map ~f:(fun seq -> Vdom.Node.tr (List.rev seq))
  ;;
end

let rec render_header header ~level ~acc ~column_widths ~set_column_width =
  let recurse = render_header ~level:(level + 1) ~column_widths ~set_column_width in
  let recurse_no_level_change = render_header ~level ~column_widths ~set_column_width in
  let colspan = attr_colspan (Header_tree.colspan header) in
  match header with
  | Leaf { visible; leaf_header; initial_width } ->
    let node index =
      let column_width =
        match Map.find column_widths index with
        | Some (Column_size.Visible { width_px = width })
        | Some (Hidden { prev_width_px = Some width }) -> `Px_float width
        | None | Some (Hidden { prev_width_px = None }) -> initial_width
      in
      Vdom.Node.td
        ~attrs:
          [ Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
              (fun ~width ~height:_ -> set_column_width ~index (`Px_float width))
          ; Vdom.Attr.colspan 1
          ; Style.header_label
          ; Style.leaf_header
          ; Vdom.Attr.style
              Css_gen.(width column_width @> if visible then empty else display `None)
          ]
        [ leaf_header ]
    in
    Acc.visit_leaf acc ~level ~node
  | Spacer inside ->
    let node = Vdom.Node.td ~attrs:[ colspan ] [] in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    recurse inside ~acc
  | Group { children; group_header } ->
    let attrs = Vdom.Attr.many [ colspan; Style.header_label ] in
    let node = Vdom.Node.td ~attrs:[ attrs ] [ group_header ] in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    List.fold children ~init:acc ~f:(fun acc -> recurse ~acc)
  | Organizational_group children ->
    List.fold children ~init:acc ~f:(fun acc -> recurse_no_level_change ~acc)
;;

let render_header headers ~column_widths ~set_column_width =
  headers
  |> render_header ~acc:Acc.empty ~level:0 ~column_widths ~set_column_width
  |> Acc.finalize
;;

let component
      (headers : Header_tree.t Value.t)
      ~column_widths
      ~set_column_width
      ~set_header_client_rect
  =
  let%arr set_column_width = set_column_width
  and set_header_client_rect = set_header_client_rect
  and headers = headers
  and column_widths = column_widths in
  let rows = render_header headers ~set_column_width ~column_widths in
  Vdom.Node.table
    ~attrs:
      [ Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:set_header_client_rect
      ; Style.partial_render_table_header
      ; Vdom.Attr.class_ "prt-table-header"
      ]
    [ Vdom.Node.tbody rows ]
;;
