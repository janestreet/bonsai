open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Incr_map_collate
open Bonsai_web_ui_partial_render_table_protocol
module Bbox = Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox
module Order = Order
module Sortable_header = Sortable_header
module Focus_by_row = Focus.By_row

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

  let implementation
        (type key presence data cmp)
        ~preload_rows
        (key : (key, cmp) Bonsai.comparator)
        ~(focus : (_, presence, key) Focus.Kind.t)
        ~row_height:(`Px row_height_px as row_height)
        ~headers
        ~assoc
        (collated : (key, data) Collated.t Value.t)
    =
    let focus_kind = focus in
    let%sub input_map = Bonsai.pure Collated.to_map_list collated in
    let%sub path = Bonsai.path_id in
    let%sub leaves = Bonsai.pure Header_tree.leaves headers in
    let%sub header_client_rect, set_header_client_rect =
      Bonsai.Incr.model_cutoff (Bonsai.state_opt (module Bbox.Int))
    in
    let%sub set_header_client_rect =
      let%arr set_header_client_rect = set_header_client_rect in
      fun b -> set_header_client_rect (Some b)
    in
    let%sub table_body_visible_rect, set_table_body_visible_rect =
      Bonsai.Incr.model_cutoff (Bonsai.state_opt (module Bbox.Int))
    in
    let%sub table_body_client_rect, set_table_body_client_rect =
      Bonsai.Incr.model_cutoff (Bonsai.state_opt (module Bbox.Int))
    in
    let%sub column_widths, set_column_width =
      Bonsai.Incr.model_cutoff
        (Bonsai.state_machine0
           (module struct
             type t = Column_size.t Int.Map.t [@@deriving sexp, equal]
           end)
           (module struct
             type t = int * [ `Px_float of float ] [@@deriving sexp, equal]
           end)
           ~default_model:Int.Map.empty
           ~apply_action:(fun ~inject:_ ~schedule_event:_ model (idx, `Px_float width) ->
             (* While checking for float equality is usually not a good idea,
                this is meant to handle the specific case when a column has
                "display:none", in which case the width will be exactly 0.0, so
                there is no concern about float rounding errors. *)
             Map.update model idx ~f:(fun prev ->
               if Float.equal width 0.0
               then (
                 match prev with
                 | None -> Hidden { prev_width_px = None }
                 | Some (Visible { width_px }) ->
                   Hidden { prev_width_px = Some width_px }
                 | Some (Hidden _ as prev) -> prev)
               else Visible { width_px = width })))
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
      match table_body_visible_rect, table_body_client_rect, header_client_rect with
      | Some { min_y = body_min_y; max_y = body_max_y; _ }, _, None ->
        (* if we don't have the header-height yet, just assume that there's
           no overlap. *)
        let low = Float.of_int body_min_y /. Float.of_int row_height_px in
        let high =
          (Float.of_int body_max_y -. Float.of_int row_height_px +. 2.)
          /. Float.of_int row_height_px
        in
        Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high))
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
        Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high))
      (* Q.E.D. *)
      | _ -> 0, 1
    in
    let%sub midpoint_of_container =
      let%arr table_body_visible_rect = table_body_visible_rect in
      match table_body_visible_rect with
      | None -> 0
      | Some rect -> (rect.max_x + rect.min_x) / 2
    in
    let%sub { focus; visually_focused } =
      let (`Px row_height) = row_height in
      Focus.component
        focus_kind
        key
        ~collated
        ~range:range_without_preload
        ~header_height:header_height_px
        ~row_height
        ~midpoint_of_container
        ~path
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
          ~visible_rect_changed:(fun bounds -> set_table_body_visible_rect (Some bounds))
      in
      let%sub total_height =
        let%arr row_count = row_count in
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
          ~attr:
            Vdom.Attr.(
              many
                [ Style.partial_render_table_body
                ; Vdom.Attr.style Css_gen.(height (`Px total_height))
                ; vis_change_attr
                ])
          [ rows ]
      in
      Vdom.Node.div
        ~attr:
          Vdom.Attr.(
            Style.partial_render_table_container
            @ Vdom.Attr.class_ ("partial-render-table-" ^ path))
        [ head; body ]
    in
    let%sub range =
      let%arr low, high = range_without_preload
      and row_count = row_count in
      let low = Int.max 0 (low - preload_rows) in
      let low =
        (* always fetch an even number of rows in order to make
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
        collate)
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

  let component
    : type key presence focus data cmp.
      ?filter:(key:key -> data:data -> bool) Value.t
      -> ?default_sort:(key * data -> key * data -> int) Value.t
      -> ?preload_rows:int
      -> (key, cmp) Bonsai.comparator
      -> focus:(focus, presence, key) Focus.t
      -> row_height:[ `Px of int ]
      -> columns:(key, data) Column_intf.with_sorter
      -> (key, data, cmp) Map.t Value.t
      -> focus Result.t Computation.t
    =
    fun ?filter
      ?default_sort
      ?(preload_rows = default_preload)
      comparator
      ~focus
      ~row_height
      ~columns
      map ->
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
        Bonsai.state (module Rank_range) ~default_model:(Collate.Which_range.To 0)
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
        let%sub order =
          let%arr sorters = sorters
          and default_sort = default_sort
          and sortable_header = sortable_header in
          Order.to_compare (Sortable_header.order sortable_header) ~sorters ~default_sort
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
          (module struct
            type t = int * int [@@deriving sexp, equal]
          end)
          viewed_range
          ~callback:
            (let%map set_rank_range = set_rank_range in
             fun (low, high) -> set_rank_range (Collate.Which_range.Between (low, high)))
      in
      let%arr { view; for_testing; range = _; focus } = result
      and num_filtered_rows = num_filtered_rows in
      { Result.view; for_testing; focus; num_filtered_rows }
  ;;
end
