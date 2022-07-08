open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Incr_map_collate

module Focus = struct
  include Focus
  include Kind
end

module For_testing = struct
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

module Expert = struct
  module Bonsai_collate = Incr_map_collate.Make (Ui_incr)
  module Focus = Focus

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
        (type key data cmp)
        ~preload_rows
        (key : (key, cmp) Bonsai.comparator)
        ~focus:focus_kind
        ~row_height:(`Px row_height_px as row_height)
        ~headers
        ~assoc
        (collated : (key, data) Collated.t Value.t)
    =
    let%sub input_map = Bonsai.pure Collated.to_map_list collated in
    let%sub path = Bonsai.path_id in
    let%sub leaves = Bonsai.pure Header_tree.leaves headers in
    let%sub bounds, set_bounds =
      Bonsai.state_opt (module Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bounds)
      |> Bonsai.Incr.model_cutoff
    in
    let%sub column_widths, set_column_width =
      Bonsai.Incr.model_cutoff
        (Bonsai.state_machine0
           (module struct
             type t = [ `Px of float ] Int.Map.t [@@deriving compare, sexp, equal]
           end)
           (module struct
             type t = int * [ `Px of float ] [@@deriving sexp, equal]
           end)
           ~default_model:Int.Map.empty
           ~apply_action:(fun ~inject:_ ~schedule_event:_ model (idx, `Px width) ->
             (* While checking for float equality is usually not a good idea,
                this is meant to handle the specific case when a column has
                "display:none", in which case the width will be exactly 0.0, so
                there is no concern about float rounding errors. *)
             if Float.equal width 0.0
             then Map.remove model idx
             else Map.set model ~key:idx ~data:(`Px width)))
    in
    let%sub set_column_width =
      let%arr set_column_width = set_column_width in
      fun ~index width -> set_column_width (index, width)
    in
    let%sub row_count = return (collated >>| Collated.num_filtered_rows) in
    let%sub range_without_preload =
      match%arr bounds with
      | None -> 0, 1
      | Some { min_y; max_y; _ } ->
        let low = min_y // row_height_px in
        let high = (max_y - row_height_px + 2) / row_height_px in
        Float.(to_int (round_up low)), high
    in
    let%sub header_height_px, set_header_height =
      Bonsai.state (module Int) ~default_model:0
    in
    let%sub rows_covered_by_header =
      let%arr header_height_px = header_height_px in
      Float.(to_int (round_up (header_height_px // row_height_px)))
    in
    let%sub focus =
      Focus.component
        focus_kind
        key
        ~collated
        ~range:range_without_preload
        ~rows_covered_by_header
        ~path
    in
    let on_row_click = Focus.get_on_row_click focus_kind focus in
    let visually_focused = Focus.get_focused focus_kind focus in
    let%sub rows, body_for_testing =
      Table_body.component
        ~comparator:key
        ~row_height
        ~leaves
        ~assoc
        ~column_widths
        ~visually_focused
        ~on_row_click
        collated
        input_map
    in
    let%sub head = Table_header.component headers ~set_column_width ~set_header_height in
    let%sub view =
      let%arr set_bounds = set_bounds
      and head = head
      and row_count = row_count
      and rows = rows
      and path = path in
      Vdom.Node.lazy_
        (lazy
          (let total_height = row_count * row_height_px in
           let body =
             Vdom.Node.div
               (* If the number is large enough, it will use scientific notation for unknown reasons.
                  However, the number is accurate, and scientific notation is in spec.
                  https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
               ~attr:
                 Vdom.Attr.(
                   class_ Style.partial_render_table_body
                   @ style Css_gen.(height (`Px total_height))
                   @ Bonsai_web_ui_element_size_hooks.Visibility_tracker.on_change
                       (fun bounds -> set_bounds (Some bounds)))
               (Lazy.force rows)
           in
           Vdom.Node.div
             ~attr:
               Vdom.Attr.(
                 Vdom.Attr.class_ Style.partial_render_table_container
                 @ Vdom.Attr.class_ ("partial-render-table-" ^ path))
             [ head; body ]))
    in
    let%sub range =
      let%arr low, high = range_without_preload
      and row_count = row_count in
      let low = Int.max 0 (low - preload_rows) in
      let high = Int.min row_count (high + preload_rows) in
      low, Int.max low high
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
        (type key data cmp)
        ?(preload_rows = 25)
        (key : (key, cmp) Bonsai.comparator)
        ~(focus : ('focus, key) Focus.Kind.t)
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
      Bonsai_collate.collate
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
  module Focus = Focus

  module Result = struct
    type 'focus t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
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

  module Order = struct
    type t = (int * [ `Asc | `Desc ]) list [@@deriving sexp, equal]

    module Action = struct
      type t =
        | Set_sort of int
        | Add_sort of int
      [@@deriving sexp]
    end

    let apply_action ~inject:_ ~schedule_event:_ model =
      let cycle_sort_direction i =
        match List.find model ~f:(fun (x, _) -> i = x) with
        | None -> [ i, `Asc ]
        | Some (_, `Asc) -> [ i, `Desc ]
        | Some (_, `Desc) -> []
      in
      function
      | Action.Set_sort i -> cycle_sort_direction i
      | Add_sort i -> cycle_sort_direction i @ List.filter model ~f:(fun (x, _) -> i <> x)
    ;;

    let to_compare ~sorters ~default_sort list =
      match list, default_sort with
      | [], None -> Incr_map_collate.Compare.Unchanged
      | [], Some compare -> Incr_map_collate.Compare.Custom_by_key_and_value { compare }
      | l, compare ->
        let l =
          List.filter_map l ~f:(fun (i, direction) ->
            Option.map (Map.find sorters i) ~f:(fun compare ->
              match direction with
              | `Asc -> compare
              | `Desc -> Comparable.reverse compare))
        in
        let l = List.append l (Option.to_list compare) in
        Incr_map_collate.Compare.Custom_by_key_and_value
          { compare = Comparable.lexicographic l }
    ;;
  end

  let component
    : type key data cmp.
      ?filter:(key:key -> data:data -> bool) Value.t
      -> ?default_sort:(key * data -> key * data -> int) Value.t
      -> ?preload_rows:int
      -> (key, cmp) Bonsai.comparator
      -> focus:('focus, key) Focus.Kind.t
      -> row_height:[ `Px of int ]
      -> columns:(key, data) Column_intf.with_sorter
      -> (key, data, cmp) Map.t Value.t
      -> 'focus Result.t Computation.t
    =
    fun ?filter
      ?default_sort
      ?(preload_rows = 20)
      comparator
      ~focus
      ~row_height
      ~columns
      map ->
      let filter =
        Option.value_map filter ~default:(Value.return None) ~f:(Value.map ~f:Option.some)
      in
      let%sub rank_range, set_rank_range =
        Bonsai.state (module Rank_range) ~default_model:(Collate.Which_range.To 0)
      in
      let%sub sort_order, change_order =
        Bonsai.Incr.model_cutoff
          (Bonsai.state_machine0
             (module Order)
             (module Order.Action)
             ~default_model:[]
             ~apply_action:Order.apply_action)
      in
      let (Y { value; vtable }) = columns in
      let module Column = (val vtable) in
      let assoc = Column.instantiate_cells value comparator in
      let default_sort =
        match default_sort with
        | None -> Value.return None
        | Some v -> v >>| Option.some
      in
      let%sub change_sort =
        let%arr change_order = change_order in
        fun add_or_replace i ->
          match add_or_replace with
          | `Add -> change_order (Add_sort i)
          | `Replace -> change_order (Set_sort i)
      in
      let%sub sorters, headers =
        Column.headers_and_sorters value ~change_sort ~sort_order
      in
      let%sub collate =
        let%sub order =
          let%arr sort_order = sort_order
          and sorters = sorters
          and default_sort = default_sort in
          Order.to_compare ~sorters ~default_sort sort_order
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
      let%arr { view; for_testing; range = _; focus } = result in
      { Result.view; for_testing; focus }
  ;;
end
