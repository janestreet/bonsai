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
    let level_map = Int.Map.add_multi level_map ~key:level ~data:(node idx) in
    { leaf_index; level_map }
  ;;

  let visit_non_leaf { level_map; leaf_index } ~level ~node =
    let level_map = Int.Map.add_multi level_map ~key:level ~data:node in
    { leaf_index; level_map }
  ;;

  let finalize { level_map; leaf_index = _ } =
    level_map |> Map.data |> List.map ~f:(fun seq -> Vdom.Node.tr (List.rev seq))
  ;;
end

let rec render_header header ~level ~acc ~set_column_width =
  let recurse = render_header ~level:(level + 1) ~set_column_width in
  let recurse_no_level_change = render_header ~level ~set_column_width in
  let colspan = attr_colspan (Header_tree.colspan header) in
  match header with
  | Leaf { visible; leaf_label; initial_width } ->
    let node index =
      let column_width = initial_width in
      Vdom.Node.td
        ~attr:
          (Vdom.Attr.many_without_merge
             [ Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
                 (fun ~width ~height:_ -> set_column_width ~index (`Px width))
             ; Bonsai_web_ui_element_size_hooks.Freeze.width
             ; Vdom.Attr.colspan 1
             ; Vdom.Attr.style
                 Css_gen.(
                   text_align `Center
                   @> user_select `None
                   @> font_weight `Bold
                   @> create ~field:"resize" ~value:"horizontal"
                   @> overflow `Hidden
                   @> width (column_width :> Length.t)
                   @> if visible then empty else display `None)
             ])
        [ leaf_label ]
    in
    Acc.visit_leaf acc ~level ~node
  | Spacer inside ->
    let node = Vdom.Node.td ~attr:colspan [] in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    recurse inside ~acc
  | Group { children; group_label } ->
    let style = Css_gen.(text_align `Center @> user_select `None @> font_weight `Bold) in
    let attrs = Vdom.Attr.many [ colspan; Vdom.Attr.style style ] in
    let node = Vdom.Node.td ~attr:attrs [ group_label ] in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    List.fold children ~init:acc ~f:(fun acc -> recurse ~acc)
  | Organizational_group children ->
    List.fold children ~init:acc ~f:(fun acc -> recurse_no_level_change ~acc)
;;

let render_header headers ~set_column_width =
  headers |> render_header ~acc:Acc.empty ~level:0 ~set_column_width |> Acc.finalize
;;

let component (headers : Header_tree.t Value.t) ~set_column_width ~set_header_height =
  return
  @@ let%map set_column_width = set_column_width
  and set_header_height = set_header_height
  and headers = headers in
  let rows = render_header headers ~set_column_width in
  Vdom.Node.table
    ~attr:
      (Vdom.Attr.many
         [ Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
             (fun ~width:_ ~height ->
                set_header_height (Float.to_int (Float.round_up height)))
         ; Vdom.Attr.style
             Css_gen.(
               position `Sticky @> z_index 99 @> create ~field:"top" ~value:"0px")
         ; Vdom.Attr.class_ "prt-table-header"
         ])
    [ Vdom.Node.tbody rows ]
;;
