open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Incr_map_collate
open Bonsai_web_ui_partial_render_table_protocol
module Bbox = Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox
module Order = Order
module Sortable_header = Sortable_header
module Focus_by_row = Focus.By_row
module Scroll = Bonsai_web_ui_scroll_utilities

module For_testing = struct
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

let default_preload = 70

module Expert = struct
  module Focus = struct
    include Focus
    include Kind
  end

  module Result = struct
    type 'focus t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      }
    [@@deriving fields]
  end

  module Columns = struct
    type ('key, 'data) t = ('key, 'data) Column_intf.t

    module Dynamic_cells = Column.Dynamic_cells
    module Dynamic_columns = Column.Dynamic_columns
  end

  module Row_height_model = struct
    type t = [ `Px of int ] [@@deriving sexp, equal]
  end

  let print_in_tests f =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_benchmark | `Node | `Node_benchmark -> Effect.Ignore
    | `Node_test -> Effect.of_sync_fun (fun () -> print_endline (f ())) ()
  ;;

  let implementation
        (type key presence data cmp)
        ~preload_rows
        (key : (key, cmp) Bonsai.comparator)
        ~(focus : (_, presence, key) Focus.Kind.t)
        ~row_height
        ~headers
        ~assoc
        (collated : (key, data) Collated.t Value.t)
    =
    let%sub row_height =
      let%arr (`Px row_height) = row_height in
      `Px (Int.max 1 row_height)
    in
    let focus_kind = focus in
    let%sub input_map = Bonsai.pure Collated.to_map_list collated in
    let%sub path = Bonsai.path_id in
    let%sub leaves = Bonsai.pure Header_tree.leaves headers in
    let%sub header_client_rect, set_header_client_rect =
      Bonsai.state_opt
        ()
        ~sexp_of_model:[%sexp_of: Bbox.Int.t]
        ~equal:[%equal: Bbox.Int.t]
    in
    let%sub header_client_rect =
      return (Value.cutoff ~equal:[%equal: Bbox.Int.t option] header_client_rect)
    in
    let%sub set_header_client_rect =
      let%arr set_header_client_rect = set_header_client_rect in
      fun b -> set_header_client_rect (Some b)
    in
    let%sub table_body_visible_rect, set_table_body_visible_rect =
      Bonsai.state_opt
        ()
        ~sexp_of_model:[%sexp_of: Bbox.Int.t]
        ~equal:[%equal: Bbox.Int.t]
    in
    let%sub table_body_visible_rect =
      return (Value.cutoff ~equal:[%equal: Bbox.Int.t option] table_body_visible_rect)
    in
    let%sub table_body_client_rect, set_table_body_client_rect =
      Bonsai.state_opt
        ()
        ~sexp_of_model:[%sexp_of: Bbox.Int.t]
        ~equal:[%equal: Bbox.Int.t]
    in
    let%sub table_body_client_rect =
      return (Value.cutoff ~equal:[%equal: Bbox.Int.t option] table_body_client_rect)
    in
    let module Column_widths_model = struct
      type t = Column_size.t Int.Map.t [@@deriving sexp, equal]
    end
    in
    let%sub column_widths, set_column_width =
      Bonsai.state_machine0
        ()
        ~sexp_of_model:[%sexp_of: Column_widths_model.t]
        ~equal:[%equal: Column_widths_model.t]
        ~sexp_of_action:[%sexp_of: int * [ `Px_float of float ]]
        ~default_model:Int.Map.empty
        ~apply_action:
          (fun
            (_ : _ Bonsai.Apply_action_context.t) model (idx, `Px_float width) ->
            (* While checking for float equality is usually not a good idea,
               this is meant to handle the specific case when a column has
               "display:none", in which case the width will be exactly 0.0, so
               there is no concern about float rounding errors. *)
            Map.update model idx ~f:(fun prev ->
              if Float.equal width 0.0
              then (
                match prev with
                | None -> Hidden { prev_width_px = None }
                | Some (Visible { width_px }) -> Hidden { prev_width_px = Some width_px }
                | Some (Hidden _ as prev) -> prev)
              else Visible { width_px = width }))
    in
    let%sub column_widths =
      return (Value.cutoff ~equal:[%equal: Column_widths_model.t] column_widths)
    in
    let%sub set_column_width =
      let%arr set_column_width = set_column_width in
      fun ~index width -> set_column_width (index, width)
    in
    let row_count = collated >>| Collated.num_filtered_rows in
    let%sub header_height_px =
      match%arr header_client_rect with
      | None -> 0
      | Some table_body_visible_rect -> Bbox.Int.height table_body_visible_rect
    in
    let%sub range_without_preload =
      (* The goal of this value is to track the index range of the rows that would be
         visible in the table if the table were full.  Usually this would be as easy as
         looking at the table_body_visible_rect, but we also account for the header occluding
         some of the first rows of the table.  For that, we need to look at the client-rects for
         the body and the header and subtract their overlap. *)
      let%arr table_body_visible_rect = table_body_visible_rect
      and table_body_client_rect = table_body_client_rect
      and header_client_rect = header_client_rect
      and header_height_px = header_height_px in
      fun (`Px row_height_px) ->
        match table_body_visible_rect, table_body_client_rect, header_client_rect with
        | Some { min_y = body_min_y; max_y = body_max_y; _ }, _, None ->
          (* if we don't have the header-height yet, just assume that there's
             no overlap. *)
          let low = Float.of_int body_min_y /. Float.of_int row_height_px in
          let high =
            (Float.of_int body_max_y -. Float.of_int row_height_px +. 2.)
            /. Float.of_int row_height_px
          in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | ( Some { min_y = body_min_y; max_y = body_max_y; _ }
          , Some { min_y = client_body_min_y; _ }
          , Some { max_y = header_max_y; _ } ) ->
          let header_overlap =
            Int.min header_height_px (header_max_y - client_body_min_y)
          in
          let low =
            Float.of_int (body_min_y + header_overlap) /. Float.of_int row_height_px
          in
          let high =
            (Float.of_int body_max_y -. Float.of_int row_height_px +. 2.)
            /. Float.of_int row_height_px
          in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | _ -> None
    in
    let%sub midpoint_of_container =
      let%arr table_body_visible_rect = table_body_visible_rect in
      match table_body_visible_rect with
      | None -> 0
      | Some rect -> (rect.max_x + rect.min_x) / 2
    in
    let%sub scroll_to_index =
      let%arr header_height_px = header_height_px
      and range_without_preload = range_without_preload
      and midpoint_of_container = midpoint_of_container
      and path = path
      and (`Px row_height_px) = row_height in
      fun index ->
        let range_start, range_end =
          (* [range_without_preload] can be [None] if the number of rows in
             the table shrinks such that the table becomes invisible. By
             providing a small index, we ensure that the padding around the
             table does not force the page to remain large enough for the
             existing scroll position. With the padding removed, the browser
             should automatically reduce the range of the scrollbar, and
             possibly bringing the table back in view, which would quickly
             correct this value to something more useful. *)
          range_without_preload (`Px row_height_px) |> Option.value ~default:(0, 1)
        in
        let to_top =
          (* scrolling this row to the top of the display involves
             scrolling to a pixel that is actually [header_height] _above_
             the target row. *)
          Some ((row_height_px * index) - header_height_px)
        in
        let to_bottom =
          (* scroll to the bottom of this row means scrolling to the top of
             a one-pixel element just below this row *)
          Some (row_height_px * (index + 1))
        in
        let y_px =
          if index <= range_start
          then to_top
          else if index >= range_end
          then to_bottom
          else None
        in
        let selector = ".partial-render-table-" ^ path ^ " > div" in
        match y_px with
        | Some y_px ->
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling to index %{index#Int} at %{y_px#Int}px"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container
            ~y_px
            ~selector
            `Minimal
          |> Effect.ignore_m
        | None ->
          print_in_tests (fun () -> "skipping scroll because target already in view")
    in
    let%sub keep_top_row_in_position =
      let%arr range_without_preload = range_without_preload
      and header_height_px = header_height_px
      and midpoint_of_container = midpoint_of_container
      and path = path
      and table_body_visible_rect = table_body_visible_rect in
      fun (`Px old_row_height_px) (`Px new_row_height_px) ->
        match range_without_preload (`Px old_row_height_px), table_body_visible_rect with
        | None, None | None, Some _ ->
          (* If there are no rows visible [range_without_preload] will return
             [None]. In this case we should do nothing because the scroll
             position is outside the tables jurisdiction. *)
          Effect.Ignore
        | Some _, None ->
          (* [range_without_preload] is expected to return a range only if the
             table body is partly visible, so this case is unexpected. We don't
             do anything except print because we need to visible rect to do
             anything correct. *)
          Effect.print_s
            [%message
              [%here]
                "BUG: the visible rect shouldn't be none when there is range of rows"]
        | Some (range_start, _range_end), Some table_body_visible_rect ->
          (* If some rows of the table are visible, we scroll such that the top
             visible row remains in the same position in the viewport. *)
          let old_y_px = old_row_height_px * range_start in
          let new_y_px = new_row_height_px * range_start in
          let selector = ".partial-render-table-" ^ path ^ " > div" in
          let y_px =
            match new_y_px < old_y_px with
            | true -> new_y_px - header_height_px
            | false ->
              new_y_px
              - header_height_px
              + (table_body_visible_rect.max_y - table_body_visible_rect.min_y)
              - 1
          in
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling position %{y_px#Int}px into view"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container
            ~y_px
            ~selector
            `Minimal
          |> Effect.ignore_m
    in
    let%sub () =
      (* If [row_height] changes, we want scrolling to follow the visible set
         of rows to their new location. To do this, we calculate the new
         position of the current top row, and then scroll their immediately. *)
      let%sub callback =
        let%arr keep_top_row_in_position = keep_top_row_in_position in
        fun prev_row_height new_row_height ->
          match prev_row_height with
          | Some prev_row_height ->
            keep_top_row_in_position prev_row_height new_row_height
          | None -> Effect.Ignore
      in
      Bonsai.Edge.on_change'
        ~sexp_of_model:[%sexp_of: Row_height_model.t]
        ~equal:[%equal: Row_height_model.t]
        row_height
        ~callback
    in
    let%sub range_without_preload =
      let%sub prev_row_height =
        Bonsai.previous_value
          ~sexp_of_model:[%sexp_of: Row_height_model.t]
          ~equal:[%equal: Row_height_model.t]
          row_height
      in
      let%arr prev_row_height = prev_row_height
      and row_height = row_height
      and range_without_preload = range_without_preload in
      (* If the [row_height] just changed, then we will be scrolling to a new
         position that will leave [range_without_preload] unchanged. Thus, we
         should intentionally _not_ account for the new change in row_height
         yet, since otherwise we will get flickering in the UI. *)
      let range =
        match prev_row_height with
        | Some prev_row_height -> range_without_preload prev_row_height
        | None -> range_without_preload row_height
      in
      Option.value range ~default:(0, 1)
    in
    let%sub { focus; visually_focused } =
      Focus.component
        focus_kind
        key
        ~collated
        ~range:range_without_preload
        ~scroll_to_index
    in
    let on_row_click = Focus.get_on_row_click focus_kind focus in
    let%sub rows, body_for_testing =
      Table_body.component
        ~comparator:key
        ~row_height
        ~leaves
        ~headers
        ~assoc
        ~column_widths
        ~visually_focused
        ~on_row_click
        collated
        input_map
    in
    let%sub head =
      Table_header.component
        headers
        ~column_widths
        ~set_column_width
        ~set_header_client_rect
    in
    let%sub view =
      let%sub vis_change_attr =
        let%arr set_table_body_visible_rect = set_table_body_visible_rect
        and set_table_body_client_rect = set_table_body_client_rect in
        Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:(fun bounds -> set_table_body_client_rect (Some bounds))
          ~visible_rect_changed:set_table_body_visible_rect
      in
      let%sub total_height =
        let%arr row_count = row_count
        and (`Px row_height_px) = row_height in
        row_count * row_height_px
      in
      let%arr head = head
      and rows = rows
      and vis_change_attr = vis_change_attr
      and total_height = total_height
      and path = path in
      let body =
        Vdom.Node.div
          (* If the number is large enough, it will use scientific notation for unknown reasons.
             However, the number is accurate, and scientific notation is in spec.
             https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
          ~attrs:
            [ Vdom.Attr.(
                many
                  [ Style.partial_render_table_body
                  ; Vdom.Attr.style Css_gen.(height (`Px total_height))
                  ; vis_change_attr
                  ])
            ]
          [ rows ]
      in
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.(
              Style.partial_render_table_container
              @ Vdom.Attr.class_ ("partial-render-table-" ^ path))
          ]
        [ head; body ]
    in
    let%sub range =
      let%arr low, high = range_without_preload
      and row_count = row_count in
      let low = Int.max 0 (low - preload_rows) in
      let low =
        (* always fetch a range starting at an even index in order to make
           css-selecting on even and odd rows work. *)
        low - (low % 2)
      in
      let high = Int.min row_count (high + preload_rows) in
      let low, high = low, Int.max low high in
      low, high
    in
    let%arr view = view
    and range = range
    and body_for_testing = body_for_testing
    and focus = focus in
    let for_testing =
      let%map.Lazy body = body_for_testing in
      { For_testing.body }
    in
    { Result.view; range; for_testing; focus }
  ;;

  let component
        (type key focus presence data cmp)
        ?(preload_rows = default_preload)
        (key : (key, cmp) Bonsai.comparator)
        ~(focus : (focus, presence, key) Focus.Kind.t)
        ~row_height
        ~(columns : (key, data) Column_intf.t)
        (collated : (key, data) Collated.t Value.t)
    =
    let (T { value; vtable }) = columns in
    let module T = (val vtable) in
    let%sub headers = T.headers value in
    let assoc = T.instantiate_cells value key in
    implementation ~preload_rows key ~focus ~row_height ~headers ~assoc collated
  ;;

  let collate
        (type k v cmp filter order)
        ?operation_order
        ~filter_equal
        ~order_equal
        ~(filter_to_predicate : filter -> _)
        ~(order_to_compare : order -> _)
        (data : (k, v, cmp) Map.t Value.t)
        (collate : (k, filter, order) Collate.t Value.t)
    =
    let data_and_collate = Value.both data collate in
    Bonsai.Incr.compute data_and_collate ~f:(fun data_and_collate ->
      let open Ui_incr.Let_syntax in
      let%pattern_bind data, collate = data_and_collate in
      Incr_map_collate.collate
        ?operation_order
        ~filter_equal
        ~order_equal
        ~filter_to_predicate
        ~order_to_compare
        data
        collate
      |> Incr_map_collate.collated)
  ;;
end

module Basic = struct
  module Focus = struct
    include Focus

    type ('a, 'p, 'k) t =
      | None : (unit, unit, 'k) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Value.t }
          -> ('k By_row.optional, 'k option, 'k) t
  end

  module Result = struct
    type 'focus t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_header : int Sortable_header.t
      }
    [@@deriving fields]
  end

  module Columns = struct
    type ('key, 'data) t = ('key, 'data) Column_intf.with_sorter

    module Dynamic_cells = Column.Dynamic_cells_with_sorter
    module Dynamic_columns = Column.Dynamic_columns_with_sorter
  end

  module Rank_range = struct
    type t = int Collate.Which_range.t [@@deriving sexp, equal]
  end

  type 'a compare = 'a -> 'a -> int

  let component
    : type key presence focus data cmp.
      ?filter:(key:key -> data:data -> bool) Value.t
      -> ?override_sort:
           (key compare -> (key * data) compare -> (key * data) compare) Value.t
      -> ?default_sort:(key * data) compare Value.t
      -> ?preload_rows:int
      -> (key, cmp) Bonsai.comparator
      -> focus:(focus, presence, key) Focus.t
      -> row_height:[ `Px of int ] Value.t
      -> columns:(key, data) Column_intf.with_sorter
      -> (key, data, cmp) Map.t Value.t
      -> focus Result.t Computation.t
    =
    fun ?filter
      ?override_sort
      ?default_sort
      ?(preload_rows = default_preload)
      comparator
      ~focus
      ~row_height
      ~columns
      map ->
      let module Cmp = (val comparator) in
      let focus : (focus, presence, key) Expert.Focus.Kind.t =
        match focus with
        | None -> None
        | By_row { on_change } ->
          let compute_presence focus =
            let%arr focus = focus
            and map = map in
            match focus with
            | None -> None
            | Some focus -> if Map.mem map focus then Some focus else None
          in
          By_row { on_change; compute_presence }
      in
      let filter =
        Option.value_map filter ~default:(Value.return None) ~f:(Value.map ~f:Option.some)
      in
      let%sub rank_range, set_rank_range =
        Bonsai.state
          (Collate.Which_range.To 0)
          ~sexp_of_model:[%sexp_of: Rank_range.t]
          ~equal:[%equal: Rank_range.t]
      in
      let%sub sortable_header = Sortable_header.component (module Int) in
      let (Y { value; vtable }) = columns in
      let module Column = (val vtable) in
      let assoc = Column.instantiate_cells value comparator in
      let default_sort =
        match default_sort with
        | None -> Value.return None
        | Some v -> v >>| Option.some
      in
      let%sub sorters, headers = Column.headers_and_sorters value sortable_header in
      let%sub collate =
        let%sub override_sort =
          match override_sort with
          | None -> Bonsai.const None
          | Some override -> return (override >>| Option.some)
        in
        let%sub order =
          let%arr sorters = sorters
          and default_sort = default_sort
          and sortable_header = sortable_header
          and override_sort = override_sort in
          let override_sort =
            Option.map override_sort ~f:(fun override_sort ->
              override_sort Cmp.comparator.compare)
          in
          Order.to_compare
            (Sortable_header.order sortable_header)
            ?override_sort
            ~sorters
            ~default_sort
        in
        let%arr filter = filter
        and order = order
        and rank_range = rank_range in
        let key_range = Collate.Which_range.All_rows in
        { Collate.filter; order; key_range; rank_range }
      in
      let%sub collated =
        Expert.collate
          ~filter_equal:phys_equal
          ~filter_to_predicate:Fn.id
          ~order_equal:phys_equal
          ~order_to_compare:Fn.id
          map
          collate
      in
      let%sub num_filtered_rows =
        let%arr collated = collated in
        Collated.num_filtered_rows collated
      in
      let%sub ({ range = viewed_range; _ } as result) =
        Expert.implementation
          ~preload_rows
          comparator
          ~focus
          ~row_height
          ~headers
          ~assoc
          collated
      in
      let%sub () =
        Bonsai.Edge.on_change
          ~sexp_of_model:[%sexp_of: int * int]
          ~equal:[%equal: int * int]
          viewed_range
          ~callback:
            (let%map set_rank_range = set_rank_range in
             fun (low, high) -> set_rank_range (Collate.Which_range.Between (low, high)))
      in
      let%arr { view; for_testing; range = _; focus } = result
      and num_filtered_rows = num_filtered_rows
      and sortable_header = sortable_header in
      { Result.view; for_testing; focus; num_filtered_rows; sortable_header }
  ;;
end
