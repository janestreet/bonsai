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
let set_or_wrap ~classes ~style =
  let open Vdom.Node in
  function
  | Element e -> Element (Element.add_style (Element.add_classes e classes) style)
  | other ->
    div
      ~attr:(Vdom.Attr.many [ Vdom.Attr.style style; Vdom.Attr.classes classes ])
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
  : (Vdom.Node.t * For_testing.t Lazy.t) Computation.t
  =
  (* Css_gen is really slow, so we need to re-use the results of all these
     functions whenever possible.  The difference between non-cached and
     cached css is the difference between 200ms stabilizations and 0.2ms
     stabiliations while scrolling.

     The reason that Css_gen is so slow is because apparently "sprintf" is
     _really_ slow. *)
  let css_all_cells =
    let open Css_gen in
    let h = (row_height :> Length.t) in
    height h @> min_height h @> max_height h
  in
  let module Cmp = (val comparator) in
  let%sub leaves_info =
    let%arr leaves = leaves in
    let%map.List { Header_tree.visible; leaf_label; _ } = leaves in
    visible, leaf_label
  in
  let%sub cells = assoc input in
  let%sub rows =
    let%sub row_css =
      let%arr column_widths = column_widths in
      let total_width =
        column_widths
        |> Map.data
        |> List.sum
             (module Float)
             ~f:(function
               | Column_size.Visible { width_px } -> width_px
               | Hidden _ -> 0.0)
      in
      let open Css_gen in
      height (row_height :> Length.t) @> width (`Px_float total_width)
    in
    let%sub calculate_css =
      let%arr column_widths = column_widths in
      let calculate_css i =
        let column_width =
          match Map.find column_widths i with
          | Some (Visible { width_px = w })
          (* use the previous width even when hidden so that the layout engine has less
             work to do when becoming visible *)
          | Some (Hidden { prev_width_px = Some w }) -> w
          | None | Some (Hidden { prev_width_px = None }) -> 0.0
        in
        let css_for_column =
          let open Css_gen in
          let w = `Px_float column_width in
          width w @> min_width w @> max_width w
        in
        css_for_column
      in
      calculate_css
    in
    let%sub css_for_columns =
      let%arr leaves_info = leaves_info
      and calculate_css = calculate_css in
      List.mapi leaves_info ~f:(fun i (is_visible, _) ->
        let css = calculate_css i in
        if is_visible then css else Css_gen.(css @> display `None))
    in
    (* This assoc is needed to zip the cells together with the css for those
       specific columns, as well as adding the 'selected' class. *)
    Bonsai.assoc
      (module Map_list.Key)
      cells
      ~f:(fun _ key_and_cells ->
        let classes_for_each_cell = [ "prt-table-cell"; Style.cell ] in
        let for_each_cell (css_for_column, content) =
          let css = Css_gen.( @> ) css_all_cells css_for_column in
          set_or_wrap content ~classes:classes_for_each_cell ~style:css
        in
        let%sub row_selected =
          let%sub key, _ = return key_and_cells in
          let%arr visually_focused = visually_focused
          and key = key in
          match visually_focused with
          | None -> false
          | Some k -> Cmp.comparator.compare k key = 0
        in
        let%arr row_selected = row_selected
        and key, cells = key_and_cells
        and css_for_columns = css_for_columns
        and on_row_click = on_row_click
        and row_css = row_css in
        let classes = [ "prt-table-row" ] in
        let classes =
          if row_selected then "prt-table-row-selected" :: classes else classes
        in
        Vdom.Node.div
          ~attr:
            (Vdom.Attr.many
               [ Vdom.Attr.classes classes
               ; Vdom.Attr.style row_css
               ; Vdom.Attr.on_click (fun _ -> on_row_click key)
               ])
          (List.map (List.zip_exn css_for_columns cells) ~f:for_each_cell))
  in
  let%sub padding_top_and_bottom =
    let%arr collated = collated in
    let (`Px row_height_px) = row_height in
    let padding_top = Collated.num_before_range collated * row_height_px in
    let padding_bottom = Collated.num_after_range collated * row_height_px in
    padding_top, padding_bottom
  in
  let%sub for_testing =
    let%arr cells = cells
    and leaves_info = leaves_info
    and collated = collated
    and visually_focused = visually_focused in
    lazy
      (let column_names = leaves_info |> List.map ~f:Tuple2.get2 in
       { For_testing.column_names
       ; cells =
           List.map (Map.to_alist cells) ~f:(fun (id, (key, view)) ->
             let selected =
               match visually_focused with
               | None -> false
               | Some k -> Cmp.comparator.compare k key = 0
             in
             { For_testing.id; selected; view })
       ; rows_before = Collated.num_before_range collated
       ; rows_after = Collated.num_after_range collated
       ; num_filtered = Collated.num_filtered_rows collated
       ; num_unfiltered = Collated.num_unfiltered_rows collated
       })
  in
  let%sub view =
    let%arr rows = rows
    and padding_top, padding_bottom = padding_top_and_bottom in
    Vdom.Node.lazy_
      (lazy
        (Vdom.Node.div
           ~attr:
             (Vdom.Attr.style
                (Css_gen.concat
                   [ Css_gen.padding_top (`Px padding_top)
                   ; Css_gen.padding_bottom (`Px padding_bottom)
                   ]))
           [ Vdom_node_with_map_children.make rows ]))
  in
  let%arr view = view
  and for_testing = for_testing in
  view, for_testing
;;
