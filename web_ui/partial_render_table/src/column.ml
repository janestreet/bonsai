open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Dynamic_cells = struct
  module T = struct
    type ('key, 'data) t =
      | Leaf of
          { leaf_label : Vdom.Node.t Value.t
          ; initial_width : [ `Px of int ]
          ; cell : key:'key Value.t -> data:'data Value.t -> Vdom.Node.t Computation.t
          ; visible : bool Value.t
          }
      | Group of
          { children : ('key, 'data) t list
          ; group_label : Vdom.Node.t Value.t
          }
      | Org_group of ('key, 'data) t list

    let rec headers = function
      | Leaf { leaf_label; visible; initial_width; cell = _ } ->
        let%map label = leaf_label
        and visible = visible in
        Header_tree.leaf ~label ~visible ~initial_width
      | Group { children; group_label } ->
        let%map label = group_label
        and children = Value.all (List.map children ~f:headers) in
        Header_tree.group ~label children
      | Org_group children ->
        let%map children = List.map children ~f:headers |> Value.all in
        Header_tree.org_group children
    ;;

    let headers t = return (headers t)
    let empty_div = Vdom.Node.div []

    let rec visible_leaves
      : type k v cmp.
        (k, Int63.t * v, cmp) Map.t Value.t
        -> (k, Int63.t * Vdom.Node.t list, cmp) Map.t
        -> (k, cmp) Bonsai.comparator
        -> (k, v) t
        -> (k, Int63.t * Vdom.Node.t list, cmp) Map.t Computation.t list
      =
      fun map empty comparator -> function
        | Leaf { cell; visible; _ } ->
          [ (if%sub visible
             then
               Bonsai.assoc comparator map ~f:(fun key data ->
                 let%sub i, data = return data in
                 let%sub r = cell ~key ~data in
                 return
                 @@ let%map i = i
                 and r = r in
                 i, [ r ])
             else (
               let f = Ui_incr.Map.map ~f:(fun (i, _) -> i, [ empty_div ]) in
               Bonsai.Incr.compute map ~f))
          ]
        | Group { children; _ } | Org_group children ->
          List.bind children ~f:(visible_leaves map empty comparator)
    ;;

    let instantiate_cells (type k cmp) t (comparator : (k, cmp) Bonsai.comparator) map =
      let module M = (val comparator) in
      let empty = Map.empty (module M) in
      visible_leaves map empty comparator t
      |> Computation.reduce_balanced ~f:(fun a b ->
        Bonsai.Incr.compute (Value.both a b) ~f:(fun a_and_b ->
          let%pattern_bind.Ui_incr a, b = a_and_b in
          Ui_incr.Map.merge a b ~f:(fun ~key:_ change ->
            match change with
            | `Left l -> Some l
            | `Right r -> Some r
            | `Both ((i, l), (_, r)) -> Some (i, l @ r))))
      |> Option.value ~default:(Bonsai.const empty)
    ;;
  end

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?(initial_width = `Px 50) ?(visible = Value.return true) ~label ~cell () =
    T.Leaf { leaf_label = label; initial_width; cell; visible }
  ;;

  let group ~label children = T.Group { group_label = label; children }
  let expand ~label child = group ~label [ child ]

  let lift : type key data. (key, data) T.t list -> (key, data) Column_intf.t =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value = T.Org_group columns in
      Column_intf.T { value; vtable = (module X) }
  ;;
end

module Dynamic_columns = struct
  module T = struct
    type ('key, 'data) t =
      | Leaf of
          { leaf_label : Vdom.Node.t
          ; initial_width : [ `Px of int ]
          ; cell : key:'key -> data:'data -> Vdom.Node.t
          ; visible : bool
          }
      | Group of
          { children : ('key, 'data) t list
          ; group_label : Vdom.Node.t
          }
      | Org_group of ('key, 'data) t list

    let rec translate = function
      | Leaf { leaf_label = label; initial_width; visible; cell = _ } ->
        Header_tree.leaf ~label ~visible ~initial_width
      | Group { children; group_label = label } ->
        let children = List.map children ~f:translate in
        Header_tree.group ~label children
      | Org_group children -> Header_tree.org_group (List.map children ~f:translate)
    ;;

    let headers t = Bonsai.pure translate t

    let rec visible_leaves structure ~key ~data =
      match structure with
      | Leaf { cell; visible; _ } -> if visible then [ cell ~key ~data ] else []
      | Org_group children | Group { children; group_label = _ } ->
        List.concat_map children ~f:(visible_leaves ~key ~data)
    ;;

    let instantiate_cells t _comparator map =
      Bonsai.Incr.compute (Bonsai.Value.both t map) ~f:(fun both ->
        let%pattern_bind.Ui_incr t, map = both in
        (* Why is this bind here ok?  Well, there is an alternative that involves
           Incr_map.mapi' which closes over visible_leaves as an incremental, but even
           in that scenario, if the set of visible_leaves changes, we're recomputing the
           whole world anyway, so it doesn't buy us anything vs this bind. *)
        let%bind.Ui_incr visible_leaves = Ui_incr.map t ~f:visible_leaves in
        Ui_incr.Map.mapi map ~f:(fun ~key ~data:(i, data) ->
          i, visible_leaves ~key ~data))
    ;;
  end

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?(initial_width = `Px 50) ?(visible = true) ~label ~cell () =
    T.Leaf { leaf_label = label; initial_width; cell; visible }
  ;;

  let group ~label children = T.Group { group_label = label; children }
  let expand ~label child = group ~label [ child ]

  let lift : type key data. (key, data) T.t list Value.t -> (key, data) Column_intf.t =
    let module X = struct
      type t = (key, data) T.t Value.t
      type nonrec key = key
      type nonrec data = data

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value =
        let%map columns = columns in
        T.Org_group columns
      in
      Column_intf.T { value; vtable = (module X) }
  ;;
end

module With_sorter (Tree : T2) (Container : T1) = struct
  type ('key, 'data) t =
    | Leaf of
        { t : ('key, 'data) Tree.t
        ; sort : ('key * 'data -> 'key * 'data -> int) Container.t
        }
    | Group of
        { build : ('key, 'data) Tree.t list -> ('key, 'data) Tree.t
        ; children : ('key, 'data) t list
        }

  let rec partition i sorters_acc ~f = function
    | Leaf { t = inside; sort } ->
      let sorters_acc = Int.Map.add_exn sorters_acc ~key:i ~data:sort in
      let t = f i sort inside in
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

let common_header_attributes f i =
  let open Vdom.Attr in
  style (Css_gen.white_space `Pre)
  @ on_click (fun mouse_event ->
    if Js_of_ocaml.Js.to_bool mouse_event##.shiftKey then f `Add i else f `Replace i)
  @ style (Css_gen.create ~field:"cursor" ~value:"pointer")
;;

module Dynamic_cells_with_sorter = struct
  module Container = struct
    type 'a t = 'a option Value.t
  end

  module T = With_sorter (Dynamic_cells.T) (Container)

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?sort ?initial_width ?visible ~label ~cell () =
    let sort =
      match sort with
      | None -> Value.return None
      | Some x -> Value.map x ~f:Option.some
    in
    T.Leaf { sort; t = Dynamic_cells.column ?initial_width ?visible ~label ~cell () }
  ;;

  let group ~label children = T.Group { build = Dynamic_cells.group ~label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let headers_and_sorters t ~change_sort ~sort_order =
      let sorters, tree =
        T.partition t ~f:(fun i sort -> function
          | Dynamic_cells.T.Leaf { leaf_label; initial_width; cell; visible } ->
            let leaf_label =
              let%map leaf_label = leaf_label
              and has_sorter = sort >>| Option.is_some
              and sorting = sort_order >>| (Fn.flip (List.Assoc.find ~equal:Int.equal)) i
              and change_sort = change_sort in
              let attr, leaf_label =
                if has_sorter
                then (
                  let attr = common_header_attributes change_sort i in
                  let precedes =
                    match sorting with
                    | None -> "◇ "
                    | Some `Asc -> "⬘ "
                    | Some `Desc -> "⬙ "
                  in
                  attr, Vdom.Node.span [ Vdom.Node.text precedes; leaf_label ])
                else Vdom.Attr.empty, leaf_label
              in
              Vdom.Node.div ~attr [ leaf_label ]
            in
            Dynamic_cells.T.Leaf { leaf_label; initial_width; cell; visible }
          | other -> (* This should never happen *) other)
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

    let instantiate_cells t =
      let _sorters, tree = T.partition t ~f:(fun _i _sort -> Fn.id) in
      Dynamic_cells.T.instantiate_cells tree
    ;;
  end

  let lift : type key data. (key, data) T.t list -> (key, data) Column_intf.with_sorter =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data

      include W
    end
    in
    fun columns ->
      let value =
        T.Group { children = columns; build = (fun c -> Dynamic_cells.T.Org_group c) }
      in
      Column_intf.Y { value; vtable = (module X) }
  ;;
end

module Dynamic_columns_with_sorter = struct
  module T = With_sorter (Dynamic_columns.T) (Option)

  type ('key, 'data) t = ('key, 'data) T.t

  let column ?sort ?initial_width ?visible ~label ~cell () =
    T.Leaf { sort; t = Dynamic_columns.column ?initial_width ?visible ~label ~cell () }
  ;;

  let group ~label children = T.Group { build = Dynamic_columns.group ~label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let headers_and_sorters t ~change_sort ~sort_order =
      let%sub sorters, tree =
        return
        @@ let%map t = t
        and change_sort = change_sort
        and sort_order = sort_order in
        let sorters, tree =
          T.partition t ~f:(fun i sorter -> function
            | Dynamic_columns.T.Leaf { leaf_label; initial_width; cell; visible } ->
              let attr, leaf_label =
                match sorter with
                | Some _ ->
                  let attr = common_header_attributes change_sort i in
                  let precedes =
                    match List.Assoc.find sort_order ~equal:Int.equal i with
                    | None -> "◇ "
                    | Some `Asc -> "⬘ "
                    | Some `Desc -> "⬙ "
                  in
                  let leaf_label =
                    Vdom.Node.span [ Vdom.Node.text precedes; leaf_label ]
                  in
                  attr, leaf_label
                | None -> Vdom.Attr.empty, leaf_label
              in
              let leaf_label = Vdom.Node.div ~attr [ leaf_label ] in
              Dynamic_columns.T.Leaf { leaf_label; initial_width; cell; visible }
            | other -> (* This should never happen *) other)
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

    let instantiate_cells t =
      let tree =
        let%map t = t in
        let _sorters, tree = T.partition t ~f:(fun _i _sort -> Fn.id) in
        tree
      in
      Dynamic_columns.T.instantiate_cells tree
    ;;
  end

  let lift
    : type key data. (key, data) T.t list Value.t -> (key, data) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data) T.t Value.t
      type nonrec key = key
      type nonrec data = data

      include W
    end
    in
    fun columns ->
      let value =
        let%map columns = columns in
        T.Group { children = columns; build = (fun c -> Dynamic_columns.T.Org_group c) }
      in
      Column_intf.Y { value; vtable = (module X) }
  ;;
end

