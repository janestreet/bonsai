open! Core
open! Bonsai_web
open! Js_of_ocaml
open! Incr_map_collate
open! Bonsai.Let_syntax

module Acc = struct
  type t = { level_map : Table_view.Header.Header_cell.t list Int.Map.t }

  let empty = { level_map = Int.Map.empty }

  let visit_leaf { level_map } ~level ~node =
    let level_map = Map.add_multi level_map ~key:level ~data:node in
    { level_map }
  ;;

  let visit_non_leaf { level_map } ~level ~node =
    let level_map = Map.add_multi level_map ~key:level ~data:node in
    { level_map }
  ;;

  let finalize ~themed_attrs { level_map } =
    level_map
    |> Map.data
    |> List.map ~f:(fun seq ->
         Table_view.Header.Header_row.view themed_attrs (List.rev seq))
  ;;
end

let rec render_header header ~themed_attrs ~level ~acc ~column_widths ~set_column_width =
  let recurse =
    render_header ~themed_attrs ~level:(level + 1) ~column_widths ~set_column_width
  in
  let recurse_no_level_change =
    render_header ~themed_attrs ~level ~column_widths ~set_column_width
  in
  match header with
  | Header_tree.Leaf { visible; leaf_header; initial_width; column_id } ->
    let node =
      let column_width =
        match Map.find column_widths column_id with
        | Some (Column_size.Visible { width_px = width })
        | Some (Hidden { prev_width_px = Some width }) -> `Px_float width
        | None | Some (Hidden { prev_width_px = None }) -> initial_width
      in
      Table_view.Header.Header_cell.leaf_view
        themed_attrs
        ~column_width
        ~set_column_width:(set_column_width ~column_id)
        ~visible
        ~label:leaf_header
        ()
    in
    Acc.visit_leaf acc ~level ~node
  | Spacer inside ->
    let node =
      Table_view.Header.Header_cell.spacer_view
        themed_attrs
        ~colspan:(Header_tree.colspan header)
        ()
    in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    recurse inside ~acc
  | Group { children; group_header } ->
    let node =
      Table_view.Header.Header_cell.group_view
        themed_attrs
        ~colspan:(Header_tree.colspan header)
        ~label:group_header
        ()
    in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    List.fold children ~init:acc ~f:(fun acc -> recurse ~acc)
  | Organizational_group children ->
    List.fold children ~init:acc ~f:(fun acc -> recurse_no_level_change ~acc)
;;

let render_header ~themed_attrs headers ~column_widths ~set_column_width =
  headers
  |> render_header ~themed_attrs ~acc:Acc.empty ~level:0 ~column_widths ~set_column_width
  |> Acc.finalize ~themed_attrs
;;

let component
  (type column_id column_id_cmp)
  ~themed_attrs
  (headers : column_id Header_tree.t Value.t)
  ~(column_widths : (column_id, Column_size.t, column_id_cmp) Map.t Value.t)
  ~(set_column_width :
      (column_id:column_id -> [< `Px_float of float ] -> unit Effect.t) Value.t)
  ~set_header_client_rect
  =
  let%arr set_column_width = set_column_width
  and set_header_client_rect = set_header_client_rect
  and headers = headers
  and column_widths = column_widths
  and themed_attrs = themed_attrs in
  let header_rows =
    render_header headers ~themed_attrs ~set_column_width ~column_widths
  in
  Table_view.Header.view themed_attrs ~set_header_client_rect header_rows
;;
