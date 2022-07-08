open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing = struct
  type cell =
    { id : Map_list.Key.t
    ; selected : bool
    ; view : Vdom.Node.t list
    }

  type t =
    { column_names : Vdom.Node.t list
    ; cells : cell list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

(* This function takes a vdom node and if it's an element, it adds extra attrs, classes, key,
   and style info to it, but if it's not an element, it wraps that node in a div that has those
   attributes, styles, key, and style.  This can be useful if you get a vdom node from the
   user of this API, and want to avoid excessive node wrapping. *)
let set_or_wrap ?key ?(attrs = []) ~classes ~style =
  let open Vdom.Node in
  function
  | Element e ->
    Option.value_map key ~f:(Element.with_key e) ~default:e
    |> Fn.flip Element.add_style style
    |> Fn.flip Element.add_classes classes
    |> Element.map_attrs ~f:(fun old_attrs -> Vdom.Attr.many (attrs @ [ old_attrs ]))
    |> Element
  | other ->
    div
      ?key
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.style style; Vdom.Attr.classes classes; Vdom.Attr.many attrs ])
      [ other ]
;;

let component
      (type key data cmp)
      ~(comparator : (key, cmp) Bonsai.comparator)
      ~row_height
      ~(leaves : Header_tree.leaf list Value.t)
      ~(assoc :
          (key * data) Map_list.t Value.t
        -> (key * Vdom.Node.t list) Map_list.t Computation.t)
      ~column_widths
      ~(visually_focused : key option Value.t)
      ~on_row_click
      (collated : (key, data) Collated.t Value.t)
      (input : (key * data) Map_list.t Value.t)
  : (Vdom.Node.t list Lazy.t * For_testing.t Lazy.t) Computation.t
  =
  let module Cmp = (val comparator) in
  let%sub leaves_info =
    let%arr leaves = leaves in
    let%map.List { Header_tree.visible; leaf_label; _ } = leaves in
    visible, leaf_label
  in
  let%sub cells = assoc input in
  let%arr cells = cells
  and collated = collated
  and leaves_info = leaves_info
  and visually_focused = visually_focused
  and on_row_click = on_row_click
  and column_widths = column_widths in
  let elements_prior_to_range = Collated.num_before_range collated in
  (* Css_gen is really slow, so we need to re-use the results of all these
     functions whenever possible.  The difference between non-cached and
     cached css is the difference between 200ms stabilizations and 0.2ms
     stabiliations while scrolling.

     The reason that Css_gen is so slow is because apparently "sprintf" is
     _really_ slow. *)
  let css_all_cells =
    let open Css_gen in
    let h = (row_height :> Length.t) in
    height h
    @> min_height h
    @> max_height h
    @> box_sizing `Border_box
    @> overflow `Hidden
    @> display `Inline_block
    @> create ~field:"contain" ~value:"strict"
  in
  (* Build up a list of tuples corresponding to columns in the table where each tuple contains
     1. The x position for the start of that column
     2. The width of that column *)
  let calculate_position_and_css i prev_x =
    let column_width =
      match Map.find column_widths i with
      | Some (`Px w) -> w
      | None -> 0.0
    in
    let next_x = prev_x +. column_width in
    let css_for_column =
      let open Css_gen in
      let w = `Px_float column_width in
      width w @> min_width w @> max_width w
    in
    next_x, css_for_column
  in
  let _end_pos, css_for_columns =
    List.fold_mapi leaves_info ~init:0.0 ~f:(fun i prev_x (is_visible, _) ->
      if is_visible
      then calculate_position_and_css i prev_x
      else prev_x, Css_gen.display `None)
  in
  let for_each_cell ~idx j (css_for_column, content) =
    (* The index assigned during collation is a good choice for the key because it's
       semi-stable, it prints out to a string without issue, and it's guaranteed to be
       unique (within a table). *)
    let key = sprintf !"key_%s-%d" idx j in
    let css = Css_gen.( @> ) css_all_cells css_for_column in
    let classes = [ "prt-table-cell" ] in
    set_or_wrap
      content
      ~key
      ~classes
      ~attrs:[ Vdom.Attr.create "data-row-id" (sprintf "key_%s" idx) ]
      ~style:css
  in
  let row_selected key =
    match visually_focused with
    | None -> false
    | Some k -> Cmp.comparator.compare k key = 0
  in
  let for_each_row i (idx, (key, columns)) =
    let row_selected = row_selected key in
    let offset = i + elements_prior_to_range in
    let (`Px row_height_px) = row_height in
    let css =
      let open Css_gen in
      top (`Px (offset * row_height_px))
      @> position `Absolute
      @> max_height (row_height :> Length.t)
      @> Css_gen.create ~field:"width" ~value:"max-content"
    in
    let classes =
      if offset % 2 = 0
      then [ "prt-table-row"; "prt-table-row-even" ]
      else [ "prt-table-row"; "prt-table-row-odd" ]
    in
    let idx = Map_list.Key.to_string idx in
    let classes = if row_selected then "prt-table-row-selected" :: classes else classes in
    Vdom.Node.div
      ~key:idx
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.classes classes
           ; Vdom.Attr.style css
           ; Vdom.Attr.on_click (fun _ -> on_row_click key)
           ])
      (List.mapi (List.zip_exn css_for_columns columns) ~f:(for_each_cell ~idx))
  in
  let view = lazy (List.mapi (Map.to_alist cells) ~f:for_each_row) in
  let for_testing =
    lazy
      (let column_names = leaves_info |> List.map ~f:Tuple2.get2 in
       { For_testing.column_names
       ; cells =
           List.map (Map.to_alist cells) ~f:(fun (id, (key, view)) ->
             { For_testing.id; selected = row_selected key; view })
       ; rows_before = Collated.num_before_range collated
       ; rows_after = Collated.num_after_range collated
       ; num_filtered = Collated.num_filtered_rows collated
       ; num_unfiltered = Collated.num_unfiltered_rows collated
       })
  in
  view, for_testing
;;
