open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing = struct
  type cell =
    { cell_focused : bool
    ; view : Vdom.Node.t
    }

  type row =
    { id : Opaque_map.Key.t
    ; row_focused : bool
    ; cells : cell list
    }

  type t =
    { column_names : Vdom.Node.t list list
    ; rows : row list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

let rows
  (type key cmp column_id column_id_cmp kind)
  ~themed_attrs
  ~(key_comparator : (key, cmp) Bonsai.comparator)
  ~(column_id_comparator : (column_id, column_id_cmp) Bonsai.comparator)
  ~row_height
  ~(leaves : column_id Header_tree.leaf list Value.t)
  ~(column_widths : (column_id, Column_size.t, column_id_cmp) Map.t Value.t)
  ~(visually_focused : (key, column_id, kind) Focus.focused Value.t)
  ~(on_cell_click : (key -> column_id -> unit Effect.t) Value.t)
  (cells : (key * (column_id * Vdom.Node.t) list) Opaque_map.t Value.t)
  =
  let module Key_cmp = (val key_comparator) in
  let module Col_cmp = (val column_id_comparator) in
  let%sub col_widths =
    Bonsai.assoc column_id_comparator column_widths ~f:(fun _ column_width ->
      match%arr column_width with
      | Visible { width_px = w } -> `Visible w
      | Hidden { prev_width_px = Some w } -> `Hidden w
      | Hidden { prev_width_px = None } -> `Hidden 0.0)
  in
  (* It's tempting to use [Bonsai.Map.unordered_fold] over the [col_widths], but some
     columns in [col_widths] may not actually be present in the table. *)
  let%sub row_width =
    let%arr col_widths = col_widths
    and leaves = leaves in
    List.sum
      (module Float)
      leaves
      ~f:(fun leaf ->
        match Map.find col_widths leaf.column_id with
        | Some (`Visible w) -> w
        | None | Some (`Hidden _) -> 0.0)
  in
  let%sub col_styles =
    let%arr themed_attrs = themed_attrs
    and (`Px row_height) = row_height
    and col_widths = col_widths
    and leaves = leaves in
    Table_view.Cell.Col_styles.create
      column_id_comparator
      ~themed_attrs
      ~row_height
      ~col_widths
      ~leaves
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
      let%sub key, cells = return key_and_cells in
      let%sub is_row_focused =
        let%arr visually_focused = visually_focused
        and key, _ = key_and_cells in
        match visually_focused with
        | Nothing_focused | Cell_focused _ -> false
        | Row_focused k -> Comparable.equal Key_cmp.comparator.compare k key
      in
      let%sub focused_column_in_row =
        let%arr visually_focused = visually_focused
        and key = key in
        match visually_focused with
        | Nothing_focused | Row_focused _ -> None
        | Cell_focused (k, c) ->
          Option.some_if (Comparable.equal Key_cmp.comparator.compare k key) c
      in
      let%sub cells =
        let%sub is_focused =
          match%arr focused_column_in_row with
          | None -> fun _ -> false
          | Some focused_column ->
            fun column_id ->
              Comparable.equal Col_cmp.comparator.compare focused_column column_id
        in
        let%arr col_styles = col_styles
        and on_cell_click = on_cell_click
        and is_focused = is_focused
        and cells = cells
        and themed_attrs = themed_attrs
        and key = key in
        let col_styles column_id = (Staged.unstage col_styles) column_id in
        List.map cells ~f:(fun (column_id, cell) ->
          Table_view.Cell.view
            themed_attrs
            ~is_focused:(is_focused column_id)
            ~col_styles:(col_styles column_id)
            ~on_cell_click:(on_cell_click key column_id)
            cell)
      in
      let%arr themed_attrs = themed_attrs
      and cells = cells
      and is_row_focused = is_row_focused
      and row_styles = row_styles in
      Table_view.Row.view themed_attrs ~styles:row_styles ~is_focused:is_row_focused cells)
;;

let component
  (type key data cmp col col_cmp kind)
  ~themed_attrs
  ~(key_comparator : (key, cmp) Bonsai.comparator)
  ~(column_id_comparator : (col, col_cmp) Bonsai.comparator)
  ~row_height
  ~(headers : col Header_tree.t Value.t)
  ~(leaves : col Header_tree.leaf list Value.t)
  ~(assoc :
      (key * data) Opaque_map.t Value.t
      -> (key * (col * Vdom.Node.t) list) Opaque_map.t Computation.t)
  ~column_widths
  ~(visually_focused : (key, col, kind) Focus.focused Value.t)
  ~on_cell_click
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
      ~key_comparator
      ~column_id_comparator
      ~row_height
      ~leaves
      ~column_widths
      ~visually_focused
      ~on_cell_click
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
       ; rows =
           List.map (Map.to_alist cells) ~f:(fun (id, (key, view)) ->
             let module Key_cmp = (val key_comparator) in
             let module Col_cmp = (val column_id_comparator) in
             let row_focused =
               match visually_focused with
               | Nothing_focused | Cell_focused _ -> false
               | Row_focused k -> Comparable.equal Key_cmp.comparator.compare k key
             in
             let cells =
               List.map view ~f:(fun (column, view) ->
                 let cell_focused =
                   match visually_focused with
                   | Nothing_focused | Row_focused _ -> false
                   | Cell_focused (k, c) ->
                     Comparable.equal Key_cmp.comparator.compare key k
                     && Comparable.equal Col_cmp.comparator.compare c column
                 in
                 { For_testing.view; cell_focused })
             in
             { For_testing.id; row_focused; cells })
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
