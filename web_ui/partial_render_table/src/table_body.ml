open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing = struct
  type cell =
    { id : Opaque_map.Key.t
    ; focused : bool
    ; view : Vdom.Node.t list
    }

  type t =
    { column_names : Vdom.Node.t list list
    ; cells : cell list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

let rows
  (type key cmp)
  ~themed_attrs
  ~(comparator : (key, cmp) Bonsai.comparator)
  ~row_height
  ~(leaves : Header_tree.leaf list Value.t)
  ~column_widths
  ~(visually_focused : key option Value.t)
  ~on_row_click
  (cells : (key * Vdom.Node.t list) Opaque_map.Key.Map.t Value.t)
  =
  let%sub col_widths =
    let%arr col_widths = column_widths
    and leaves = leaves in
    List.mapi leaves ~f:(fun i _ ->
      match Map.find col_widths i with
      | Some (Column_size.Visible { width_px = w }) -> `Visible w
      | Some (Hidden { prev_width_px = Some w }) -> `Hidden w
      | None | Some (Hidden { prev_width_px = None }) -> `Hidden 0.0)
  in
  let%sub row_width =
    let%arr col_widths = col_widths in
    List.fold col_widths ~init:0.0 ~f:(fun acc -> function
      | `Visible w -> acc +. w
      | `Hidden _ -> acc)
  in
  let%sub cols_visible =
    let%arr leaves = leaves in
    List.map leaves ~f:(fun leaf -> leaf.visible)
  in
  let%sub col_styles =
    let%arr (`Px row_height) = row_height
    and cols_visible = cols_visible
    and col_widths = col_widths
    and themed_attrs = themed_attrs in
    Table_view.Cell.Col_styles.create ~themed_attrs ~row_height ~col_widths ~cols_visible
  in
  let%sub row_styles =
    let%arr (`Px row_height) = row_height
    and row_width = row_width in
    Table_view.Row.Styles.create ~row_height ~row_width
  in
  Bonsai.assoc
    (module Opaque_map.Key)
    cells
    ~f:(fun _ key_and_cells ->
      let%sub is_focused =
        let%arr visually_focused = visually_focused
        and key, _ = key_and_cells in
        let module Cmp = (val comparator) in
        match visually_focused with
        | None -> false
        | Some k -> Cmp.comparator.compare k key = 0
      in
      let%sub cells =
        let%arr _, cell_contents = key_and_cells
        and col_styles = col_styles in
        List.mapi cell_contents ~f:(fun i content ->
          let col_styles = col_styles i in
          Table_view.Cell.view ~col_styles content)
      in
      let%arr themed_attrs = themed_attrs
      and key, _ = key_and_cells
      and cells = cells
      and is_focused = is_focused
      and row_styles = row_styles
      and on_row_click = on_row_click in
      let on_row_click = on_row_click key in
      Table_view.Row.view themed_attrs ~styles:row_styles ~is_focused ~on_row_click cells)
;;

let component
  (type key data cmp)
  ~themed_attrs
  ~(comparator : (key, cmp) Bonsai.comparator)
  ~row_height
  ~(leaves : Header_tree.leaf list Value.t)
  ~(headers : Header_tree.t Value.t)
  ~(assoc :
      (key * data) Opaque_map.t Value.t
      -> (key * Vdom.Node.t list) Opaque_map.t Computation.t)
  ~column_widths
  ~(visually_focused : key option Value.t)
  ~on_row_click
  (collated : (key, data) Collated.t Value.t)
  (input : (key * data) Opaque_map.t Value.t)
  : (Table_view.Body.t * For_testing.t Lazy.t) Computation.t
  =
  let%sub padding_top_and_bottom =
    let%arr collated = collated
    and (`Px row_height) = row_height in
    let padding_top = Collated.num_before_range collated * row_height in
    let padding_bottom = Collated.num_after_range collated * row_height in
    padding_top, padding_bottom
  in
  let%sub cells = assoc input in
  let%sub rows =
    rows
      ~themed_attrs
      ~comparator
      ~row_height
      ~leaves
      ~column_widths
      ~visually_focused
      ~on_row_click
      cells
  in
  let%sub view =
    let%arr rows = rows
    and padding_top, padding_bottom = padding_top_and_bottom
    and themed_attrs = themed_attrs in
    Table_view.Body.view themed_attrs ~padding_top ~padding_bottom ~rows
  in
  let%sub for_testing =
    let%arr cells = cells
    and collated = collated
    and visually_focused = visually_focused
    and headers = headers in
    lazy
      (let column_names = Header_tree.column_names headers in
       { For_testing.column_names
       ; cells =
           List.map (Map.to_alist cells) ~f:(fun (id, (key, view)) ->
             let focused =
               let module Cmp = (val comparator) in
               match visually_focused with
               | None -> false
               | Some k -> Cmp.comparator.compare k key = 0
             in
             { For_testing.id; focused; view })
       ; rows_before = Collated.num_before_range collated
       ; rows_after = Collated.num_after_range collated
       ; num_filtered = Collated.num_filtered_rows collated
       ; num_unfiltered = Collated.num_unfiltered_rows collated
       })
  in
  let%arr view = view
  and for_testing = for_testing in
  view, for_testing
;;
