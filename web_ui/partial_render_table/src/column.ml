open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Sort_kind = Column_intf.Sort_kind
module Sort_state = Bonsai_web_ui_partial_render_table_protocol.Sort_state

module Indexed_column_id = struct
  type t = int [@@deriving sexp, equal]

  let to_int = Fn.id
  let of_int = Fn.id
end

module Dynamic_cells = struct
  module T = struct
    type ('key, 'data) t =
      | Leaf of
          { leaf_header : Vdom.Node.t Value.t
          ; initial_width : Css_gen.Length.t
          ; cell : key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t
          ; visible : bool Value.t
          }
      | Group of
          { children : ('key, 'data) t list
          ; group_header : Vdom.Node.t Value.t
          }
      | Org_group of ('key, 'data) t list

    let headers t =
      let rec loop ~next_id = function
        | Leaf { leaf_header; visible; initial_width; cell = _ } ->
          let tree =
            let%map header = leaf_header
            and visible = visible in
            Header_tree.leaf
              ~header
              ~visible
              ~initial_width
              ~column_id:(Indexed_column_id.of_int next_id)
          in
          tree, next_id + 1
        | Group { children; group_header } ->
          let next_id, children =
            List.fold_map ~init:next_id children ~f:(fun next_id child ->
              let child, next_id = loop ~next_id child in
              next_id, child)
          in
          let tree =
            let%map header = group_header
            and children = Value.all children in
            Header_tree.group ~header children
          in
          tree, next_id
        | Org_group children ->
          let next_id, children =
            List.fold_map ~init:next_id children ~f:(fun next_id child ->
              let child, next_id = loop ~next_id child in
              next_id, child)
          in
          let tree =
            let%map children = Value.all children in
            Header_tree.org_group children
          in
          tree, next_id
      in
      let tree, _ = loop ~next_id:0 t in
      tree
    ;;

    let headers t = return (headers t)

    let visible_leaves map comparator t =
      let rec loop
        : type k v cmp.
          next_id:int
          -> (k * v) Opaque_map.t Value.t
          -> (k, cmp) Bonsai.comparator
          -> (k, v) t
          -> ((k * Vdom.Node.t) Opaque_map.t Computation.t * Indexed_column_id.t) list
             * int
        =
        fun ~next_id map comparator -> function
        | Leaf { cell; visible; _ } ->
          let leaf =
            if%sub visible
            then
              Bonsai.Expert.assoc_on
                (module Opaque_map.Key)
                comparator
                map
                ~get_model_key:(fun _ (k, _) -> k)
                ~f:(fun _ data ->
                  let%sub key, data = return data in
                  let%sub r = cell ~key ~data in
                  let%arr key = key
                  and r = r in
                  key, r)
            else (
              let f = Ui_incr.Map.map ~f:(fun (k, _) -> k, Vdom.Node.none) in
              Bonsai.Incr.compute map ~f)
          in
          [ leaf, Indexed_column_id.of_int next_id ], next_id + 1
        | Group { children; _ } | Org_group children ->
          let next_id, leaves =
            List.fold_map children ~init:next_id ~f:(fun next_id child ->
              let leaves, next_id = loop ~next_id map comparator child in
              next_id, leaves)
          in
          let leaves = List.concat leaves in
          leaves, next_id
      in
      let leaves, _ = loop ~next_id:0 map comparator t in
      leaves
    ;;

    let instantiate_cells (type k) t comparator (map : (k * _) Opaque_map.t Value.t) =
      let empty = Map.empty (module Opaque_map.Key) in
      visible_leaves map comparator t
      |> List.fold_right ~init:(Bonsai.const empty) ~f:(fun (leaf_comp, column_id) acc ->
           let%sub a = leaf_comp in
           let%sub acc = acc in
           Bonsai.Incr.compute (Value.both a acc) ~f:(fun a_and_acc ->
             let%pattern_bind.Ui_incr a, acc = a_and_acc in
             Ui_incr.Map.merge a acc ~f:(fun ~key:_ change ->
               match change with
               | `Left (i, l) -> Some (i, [ column_id, l ])
               | `Right (i, r) -> Some (i, r)
               | `Both ((i, l), (_, r)) -> Some (i, (column_id, l) :: r))))
    ;;
  end

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?(initial_width = `Px 50) ?(visible = Value.return true) ~header ~cell () =
    T.Leaf { leaf_header = header; initial_width; cell; visible }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  let lift
    : type key data.
      (key, data) T.t list -> (key, data, Indexed_column_id.t) Column_intf.t
    =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = Indexed_column_id.t

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value = T.Org_group columns in
      Column_intf.T { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_experimental = struct
  module T = struct
    type ('key, 'data, 'column_id, 'column_id_cmp) t =
      { column_id : ('column_id, 'column_id_cmp) Bonsai.comparator
      ; columns : 'column_id list Value.t
      ; render_header : 'column_id Value.t -> Vdom.Node.t Computation.t
      ; render_cell :
          'column_id Value.t -> 'key Value.t -> 'data Value.t -> Vdom.Node.t Computation.t
      }

    let headers
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t -> column_id Header_tree.t Computation.t
      =
      fun { column_id; columns; render_header; render_cell = _ } ->
      let module Col_id = (val column_id) in
      let%sub rendered =
        Bonsai.assoc_list
          (module Col_id)
          columns
          ~get_key:Fn.id
          ~f:(fun _ column_id ->
            let%sub header = render_header column_id in
            let%arr header = header
            and column_id = column_id in
            Header_tree.leaf ~header ~visible:true ~initial_width:(`Px 50) ~column_id)
      in
      match%sub rendered with
      | `Duplicate_key _ ->
        raise_s
          [%message
            "BUG" [%here] "should be impossible because columns were already deduplicated"]
      | `Ok headers ->
        let%arr headers = headers in
        Header_tree.org_group headers
    ;;

    let instantiate_cells
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> (key, _) Bonsai.comparator
        -> (key * data) Opaque_map.t Value.t
        -> (key * (column_id * Vdom.Node.t) list) Opaque_map.t Computation.t
      =
      fun { column_id; columns; render_header = _; render_cell } key_cmp rows ->
      let module Col_id = (val column_id) in
      Bonsai.Expert.assoc_on
        (module Opaque_map.Key)
        key_cmp
        rows
        ~get_model_key:(fun _ (k, _) -> k)
        ~f:(fun _key data ->
          let%sub key, data = return data in
          let%sub rendered =
            Bonsai.assoc_list
              (module Col_id)
              columns
              ~get_key:Fn.id
              ~f:(fun _ col ->
                let%sub cell = render_cell col key data in
                let%arr col = col
                and cell = cell in
                col, cell)
          in
          match%sub rendered with
          | `Duplicate_key _ ->
            raise_s
              [%message
                "BUG"
                  [%here]
                  "should be impossible because columns were already deduplicated"]
          | `Ok cells ->
            let%arr cells = cells
            and key = key in
            key, cells)
    ;;
  end

  let build
    : type key data column column_cmp.
      (column, column_cmp) Bonsai.comparator
      -> columns:column list Value.t
      -> render_header:(column Value.t -> Vdom.Node.t Computation.t)
      -> render_cell:
           (column Value.t -> key Value.t -> data Value.t -> Vdom.Node.t Computation.t)
      -> (key, data, column) Column_intf.t
    =
    let module X = struct
      type t = (key, data, column, column_cmp) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = column

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun column_id ~columns ~render_header ~render_cell ->
      let module Col_id = (val column_id) in
      let columns =
        let%map columns = columns in
        (* deduplicate *)
        let seen = ref (Set.empty (module Col_id)) in
        List.filter columns ~f:(fun column ->
          if Set.mem !seen column
          then false
          else (
            seen := Set.add !seen column;
            true))
      in
      let value = { T.column_id; columns; render_header; render_cell } in
      Column_intf.T { value; vtable = (module X); column_id }
  ;;

  module Sortable = Sortable
end

module Dynamic_columns = struct
  module T = struct
    type ('key, 'data) t =
      | Leaf of
          { leaf_header : Vdom.Node.t
          ; initial_width : Css_gen.Length.t
          ; cell : key:'key -> data:'data -> Vdom.Node.t
          ; visible : bool
          }
      | Group of
          { children : ('key, 'data) t list
          ; group_header : Vdom.Node.t
          }
      | Org_group of ('key, 'data) t list

    let translate t =
      let rec map_children ~next_id children =
        List.fold_map children ~init:next_id ~f:(fun next_id child ->
          let tree, next_id = loop ~next_id child in
          next_id, tree)
      and loop ~next_id = function
        | Leaf { leaf_header = header; initial_width; visible; cell = _ } ->
          ( Header_tree.leaf
              ~header
              ~visible
              ~initial_width
              ~column_id:(Indexed_column_id.of_int next_id)
          , next_id + 1 )
        | Group { children; group_header = header } ->
          let next_id, tree = map_children ~next_id children in
          Header_tree.group ~header tree, next_id
        | Org_group children ->
          let next_id, tree = map_children ~next_id children in
          Header_tree.org_group tree, next_id
      in
      let tree, _ = loop ~next_id:0 t in
      tree
    ;;

    let headers t = Bonsai.pure translate t

    let rec visible_leaves idx structure ~key ~data =
      match structure with
      | Leaf { cell; visible; _ } ->
        if visible
        then [ Indexed_column_id.of_int idx, cell ~key ~data ]
        else [ Indexed_column_id.of_int idx, Vdom.Node.none ]
      | Org_group children | Group { children; group_header = _ } ->
        List.concat_mapi children ~f:(fun i child ->
          visible_leaves (idx + i) child ~key ~data)
    ;;

    let instantiate_cells t _comparator map =
      Bonsai.Incr.compute (Bonsai.Value.both t map) ~f:(fun both ->
        let%pattern_bind.Ui_incr t, map = both in
        (* Why is this bind here ok?  Well, there is an alternative that involves
           Incr_map.mapi' which closes over visible_leaves as an incremental, but even
           in that scenario, if the set of visible_leaves changes, we're recomputing the
           whole world anyway, so it doesn't buy us anything vs this bind. *)
        let%bind.Ui_incr visible_leaves = Ui_incr.map t ~f:(visible_leaves 0) in
        Ui_incr.Map.map map ~f:(fun (key, data) -> key, visible_leaves ~key ~data))
    ;;
  end

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?(initial_width = `Px 50) ?(visible = true) ~header ~cell () =
    T.Leaf { leaf_header = header; initial_width; cell; visible }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  let lift
    : type key data.
      (key, data) T.t list Value.t -> (key, data, Indexed_column_id.t) Column_intf.t
    =
    let module X = struct
      type t = (key, data) T.t Value.t
      type nonrec key = key
      type nonrec data = data
      type column_id = Indexed_column_id.t

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value =
        let%map columns = columns in
        T.Org_group columns
      in
      Column_intf.T { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module With_sorter (Tree : T2) (Dynamic_or_static : T1) = struct
  type ('key, 'data) t =
    | Leaf of
        { t : Vdom.Node.t Dynamic_or_static.t -> ('key, 'data) Tree.t
        ; sort : ('key, 'data) Sort_kind.t option Dynamic_or_static.t
        ; header : (Sort_state.t -> Vdom.Node.t) Dynamic_or_static.t
        }
    | Group of
        { build : ('key, 'data) Tree.t list -> ('key, 'data) Tree.t
        ; children : ('key, 'data) t list
        }

  let rec partition i sorters_acc ~f = function
    | Leaf { t = inside; sort; header } ->
      let sorters_acc = Map.add_exn (sorters_acc : _ Int.Map.t) ~key:i ~data:sort in
      let t = f i inside sort header in
      let i = i + 1 in
      i, sorters_acc, t
    | Group { build; children } ->
      let (i, sorters_acc), children =
        List.fold_map children ~init:(i, sorters_acc) ~f:(fun (i, sorters_acc) child ->
          let i, sorters_acc, child = partition i sorters_acc ~f child in
          (i, sorters_acc), child)
      in
      i, sorters_acc, build children
  ;;

  let partition t ~f =
    let _, sorters, tree = partition 0 Int.Map.empty ~f t in
    sorters, tree
  ;;
end

module Dynamic_cells_with_sorter = struct
  module T = With_sorter (Dynamic_cells.T) (Value)

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?sort ?sort_reversed ?initial_width ?visible ~header ~cell () =
    let sort =
      match sort, sort_reversed with
      | None, None -> Value.return None
      | Some forward, Some reverse ->
        let%map forward = forward
        and reverse = reverse in
        Some { Sort_kind.forward; reverse }
      | Some forward, None ->
        let%map forward = forward in
        Some (Sort_kind.reversible ~forward)
      | None, Some reverse ->
        let%map reverse = reverse in
        Some (Sort_kind.reversible' ~reverse)
    in
    let t node = Dynamic_cells.column ?initial_width ?visible ~header:node ~cell () in
    T.Leaf { sort; t; header }
  ;;

  let group ~label children = T.Group { build = Dynamic_cells.group ~label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let headers_and_sorters t sortable_state =
      let sorters, tree =
        T.partition t ~f:(fun i col sort render_header ->
          let leaf_header =
            let%map render_header = render_header
            and sortable_state = sortable_state
            and sort = sort in
            Sortable.Header.Expert.default_click_handler
              ~sortable:(Option.is_some sort)
              ~column_id:i
              sortable_state
              render_header
          in
          col leaf_header)
      in
      let sorters =
        sorters
        |> Map.to_alist
        |> List.map ~f:(fun (i, sorter) ->
             let%map sorter = sorter in
             Option.map sorter ~f:(fun sorter -> i, sorter))
        |> Value.all
        >>| Fn.compose Int.Map.of_alist_exn List.filter_opt
      in
      let%sub headers = Dynamic_cells.T.headers tree in
      return (Value.both sorters headers)
    ;;

    let instantiate_cells t cmp map =
      let _sorters, tree =
        T.partition t ~f:(fun _i col sort render_header ->
          let header_node =
            let%map render_header = render_header
            and sort = sort in
            let sort_state : Sort_state.t =
              if Option.is_none sort then Not_sortable else Not_sorted
            in
            render_header sort_state
          in
          col header_node)
      in
      Dynamic_cells.T.instantiate_cells tree cmp map
    ;;
  end

  let lift
    : type key data.
      (key, data) T.t list -> (key, data, Indexed_column_id.t) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = int
      type column_id_cmp = Int.comparator_witness

      include W
    end
    in
    fun columns ->
      let value =
        T.Group { children = columns; build = (fun c -> Dynamic_cells.T.Org_group c) }
      in
      Column_intf.Y { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_columns_with_sorter = struct
  module Static = struct
    type 'a t = 'a
  end

  module T = With_sorter (Dynamic_columns.T) (Static)

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?sort ?sort_reversed ?initial_width ?visible ~header ~cell () =
    let sort =
      match sort, sort_reversed with
      | None, None -> None
      | Some forward, Some reverse -> Some { Sort_kind.forward; reverse }
      | Some forward, None -> Some (Sort_kind.reversible ~forward)
      | None, Some reverse -> Some (Sort_kind.reversible' ~reverse)
    in
    let t node = Dynamic_columns.column ?initial_width ?visible ~header:node ~cell () in
    T.Leaf { sort; header; t }
  ;;

  let group ~label children = T.Group { build = Dynamic_columns.group ~label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let headers_and_sorters t sortable_state =
      let%sub sorters, tree =
        let%arr t = t
        and sortable_state = sortable_state in
        let sorters, tree =
          T.partition t ~f:(fun i col sort render_header ->
            let sortable = Option.is_some sort in
            let header_node =
              Sortable.Header.Expert.default_click_handler
                ~sortable
                ~column_id:i
                sortable_state
                render_header
            in
            col header_node)
        in
        let sorters =
          sorters
          |> Map.to_alist
          |> List.filter_map ~f:(fun (i, sorter) ->
               Option.map sorter ~f:(fun sorter -> i, sorter))
          |> Int.Map.of_alist_exn
        in
        sorters, tree
      in
      let%sub headers = Dynamic_columns.T.headers tree in
      return (Value.both sorters headers)
    ;;

    let instantiate_cells t cmp map =
      let tree =
        let%map t = t in
        let _sorters, tree =
          T.partition t ~f:(fun _i col sort render_header ->
            let sort_state : Sort_state.t =
              if Option.is_none sort then Not_sortable else Not_sorted
            in
            col (render_header sort_state))
        in
        tree
      in
      Dynamic_columns.T.instantiate_cells tree cmp map
    ;;
  end

  let lift
    : type key data.
      (key, data) T.t list Value.t
      -> (key, data, Indexed_column_id.t) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data) T.t Value.t
      type nonrec key = key
      type nonrec data = data
      type column_id = int
      type column_id_cmp = Int.comparator_witness

      include W
    end
    in
    fun columns ->
      let value =
        let%map columns = columns in
        T.Group { children = columns; build = (fun c -> Dynamic_columns.T.Org_group c) }
      in
      Column_intf.Y { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_experimental_with_sorter = struct
  module T = struct
    type ('key, 'data, 'column_id, 'column_id_cmp) t =
      { column_id : ('column_id, 'column_id_cmp) Bonsai.comparator
      ; columns : ('column_id list * ('column_id, 'column_id_cmp) Set.t) Value.t
      ; render_header : 'column_id Value.t -> (Sort_state.t -> Vdom.Node.t) Computation.t
      ; render_cell :
          'column_id Value.t -> 'key Value.t -> 'data Value.t -> Vdom.Node.t Computation.t
      ; sorts :
          ('column_id Value.t -> ('key, 'data) Sort_kind.t option Computation.t) option
      }

    let headers_and_sorters
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> column_id Sortable.t Value.t
        -> ((column_id, (key, data) Sort_kind.t, column_id_cmp) Map.t
           * column_id Header_tree.t)
           Computation.t
      =
      fun { column_id; columns; render_header; sorts; render_cell = _ } sortable_header ->
      let module Col_id = (val column_id) in
      let%sub columns, columns_as_a_set = return columns in
      let%sub sorts =
        match sorts with
        | None -> Bonsai.const (Map.empty (module Col_id))
        | Some sorts ->
          let%sub sorts =
            Bonsai.assoc_set
              (module Col_id)
              columns_as_a_set
              ~f:(fun column -> sorts column)
          in
          Bonsai.Map.filter_map sorts ~f:Fn.id
      in
      let%sub headers =
        Bonsai.assoc_list
          (module Col_id)
          columns
          ~get_key:Fn.id
          ~f:(fun _ col ->
            let%sub render_header = render_header col in
            let%arr render_header = render_header
            and sortable_header = sortable_header
            and sorts = sorts
            and col = col in
            let sortable = Map.mem sorts col in
            let header =
              Sortable.Header.Expert.default_click_handler
                ~sortable
                ~column_id:col
                sortable_header
                render_header
            in
            Header_tree.leaf ~column_id:col ~header ~visible:true ~initial_width:(`Px 50))
      in
      let%sub headers =
        match%sub headers with
        | `Duplicate_key _ ->
          (* impossible because we already deduped *)
          assert false
        | `Ok headers ->
          let%arr headers = headers in
          Header_tree.org_group headers
      in
      let%arr headers = headers
      and sorts = sorts in
      sorts, headers
    ;;

    let instantiate_cells
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> (key, _) Bonsai.comparator
        -> (key * data) Opaque_map.t Value.t
        -> (key * (column_id * Vdom.Node.t) list) Opaque_map.t Computation.t
      =
      fun { column_id; columns; render_cell; render_header = _; sorts = _ } key_cmp rows ->
      let module Col_id = (val column_id) in
      let%sub columns, _ = return columns in
      Bonsai.Expert.assoc_on
        (module Opaque_map.Key)
        key_cmp
        rows
        ~get_model_key:(fun _ (k, _) -> k)
        ~f:(fun _key_key data ->
          let%sub key, data = return data in
          let%sub rendered =
            Bonsai.assoc_list
              (module Col_id)
              columns
              ~get_key:Fn.id
              ~f:(fun _ col ->
                let%sub cell = render_cell col key data in
                let%arr col = col
                and cell = cell in
                col, cell)
          in
          match%sub rendered with
          | `Duplicate_key _ ->
            (* impossible because we already deduped *)
            assert false
          | `Ok cells ->
            let%arr cells = cells
            and key = key in
            key, cells)
    ;;
  end

  let build
    : type key data column_id column_id_cmp.
      ?sorts:(column_id Value.t -> (key, data) Sort_kind.t option Computation.t)
      -> (column_id, column_id_cmp) Bonsai.comparator
      -> columns:column_id list Value.t
      -> render_header:(column_id Value.t -> (Sort_state.t -> Vdom.Node.t) Computation.t)
      -> render_cell:
           (column_id Value.t -> key Value.t -> data Value.t -> Vdom.Node.t Computation.t)
      -> (key, data, column_id) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data, column_id, column_id_cmp) T.t
      type nonrec key = key
      type nonrec data = data
      type nonrec column_id = column_id
      type nonrec column_id_cmp = column_id_cmp

      let headers_and_sorters = T.headers_and_sorters
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun ?sorts column_id ~columns ~render_header ~render_cell ->
      let module Col_id = (val column_id) in
      let columns =
        let%map columns = columns in
        (* deduplicate *)
        let seen = ref (Set.empty (module Col_id)) in
        let as_list =
          List.filter columns ~f:(fun column ->
            if Set.mem !seen column
            then false
            else (
              seen := Set.add !seen column;
              true))
        in
        as_list, !seen
      in
      let value = { T.column_id; columns; render_header; render_cell; sorts } in
      Column_intf.Y { value; vtable = (module X); column_id = (module Col_id) }
  ;;

  module Sortable = Sortable
end
