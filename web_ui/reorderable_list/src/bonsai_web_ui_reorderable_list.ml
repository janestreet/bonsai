open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Node = Vdom.Node
module Attr = Vdom.Attr

let transform_xy ~left ~right x y =
  Attr.many
    [ Attr.style
        (Css_gen.create
           ~field:"transform"
           ~value:[%string "translateY(%{y#Int}px) translateX(%{x#Int}px)"])
    ; Attr.style (Css_gen.position ~top:(`Px 0) ~left ~right `Absolute)
    ]
;;

type 'a t =
  | In_list of int
  | External of 'a
[@@deriving sexp]

module Array = struct
  include Array

  let fold_map_inplace t ~init ~f =
    let acc = ref init in
    for i = 0 to length t - 1 do
      let x = t.(i) in
      let new_acc, y = f !acc x in
      acc := new_acc;
      unsafe_set t i y
    done;
    !acc
  ;;
end

(* We need to collect a bunch of info related to positioning for each item.
   This type is only used internally in the list component, so we could have
   kept all these fields in a large tuple and avoided defining a new type.
   However, there are enough fields that keeping them all in a tuple is
   confusing. *)
type 'a item_positioning_info =
  { data : 'a
  ; index : int
  ; y_position : int
  ; adjusted : int
  ; size : int
  ; is_dragged_item : bool
  ; is_target_of_external_item : bool
  }

type model_info_at_index =
  { is_dragged_item : bool
  ; is_target_of_external_item : bool
  ; adjusted_index : int
  }

module type Comparator = sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module Comparator with type t = 'k and type comparator_witness = 'cmp)

(* The implementation of this function is nuanced, so it deserves a high-level
   explanation.

   Each item in the list is a draggable item and has a corresponding drop
   target at its position. Dragging an item from one position to another causes
   the items between to shift up or down to make room.

   Dragging an item from outside the list into the list is special. Dropping it
   in an items target causes that item and everything below it to shift
   downward out of the way. Since dropping the new item on the bottom item's
   target only places it in the second-to-bottom position, we have to add
   a special "extra target" to the list to enable appending new items to the
   bottom. *)
let list
  (type src cmp)
  (key : (src, cmp) comparator)
  ~(dnd : (src, int) Bonsai_web_ui_drag_and_drop.t Bonsai.Value.t)
  ?(enable_debug_overlay = false)
  ?(extra_item_attrs = Bonsai.Value.return Attr.empty)
  ?(left = `Px 0)
  ?(right = `Px 0)
  ?(empty_list_placeholder = fun ~item_is_hovered:_ -> Bonsai.const Node.None)
  ?(default_item_height = 50)
  input
  =
  let module Key = (val key) in
  let%sub sizes, size_attr =
    Bonsai_web_ui_element_size_hooks.Bulk_size_tracker.component (module Key) Prune_stale
  in
  let%sub historical_min_index =
    let%sub historical_min_index, set_historical_min_index =
      Bonsai.state Int.max_value ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t]
    in
    let%sub min_index =
      Bonsai.Incr.compute input ~f:(fun map ->
        map
        |> Incr_map.map_min ~comparator:(module Int) ~f:(fun (_, index) -> index)
        |> Incr.map ~f:(Option.value ~default:Int.max_value))
    in
    let%sub update_min =
      let%arr historical_min_index = historical_min_index
      and set_historical_min_index = set_historical_min_index in
      fun min_index ->
        if min_index < historical_min_index
        then set_historical_min_index min_index
        else Effect.Ignore
    in
    let%sub () =
      Bonsai.Edge.on_change
        ~sexp_of_model:[%sexp_of: Int.t]
        ~equal:[%equal: Int.t]
        min_index
        ~callback:update_min
    in
    return historical_min_index
  in
  let%sub model, should_render_extra_target =
    let%arr model = dnd >>| Drag_and_drop.model
    and input = input in
    match model with
    | Not_dragging -> Drag_and_drop.Model.Not_dragging, false
    | Dragging t ->
      (match Map.find input t.source with
       | Some (_, source) -> Dragging { t with source = In_list source }, false
       | None -> Dragging { t with source = External t.source }, true)
  in
  let%sub model_info_at_index =
    let%arr model = model
    and historical_min_index = historical_min_index in
    fun index ->
      let is_dragged_item =
        match model with
        | Dragging { source = In_list source; _ } -> source = index
        | Not_dragging | Dragging { source = External _; _ } -> false
      in
      match model with
      | Not_dragging | Dragging { target = None; source = External _; _ } ->
        { adjusted_index = index; is_target_of_external_item = false; is_dragged_item }
      | Dragging { target = None; source = In_list source } ->
        let target = Int.max_value in
        { adjusted_index =
            (if source = index
             then target
             else if source = target
             then index
             else if source < target
             then if source < index && index <= target then index - 1 else index
             else if source > index && index >= target
             then index + 1
             else index)
        ; is_dragged_item
        ; is_target_of_external_item = false
        }
      | Dragging { source = In_list source; target = Some target; _ } ->
        (* Target is defaulted to Int.max_value so that the elements in the list are
           compacted. *)
        let target = if target < historical_min_index then Int.max_value else target in
        { adjusted_index =
            (if source = index
             then target
             else if source = target
             then index
             else if source < target
             then if source < index && index <= target then index - 1 else index
             else if source > index && index >= target
             then index + 1
             else index)
        ; is_dragged_item
        ; is_target_of_external_item = false
        }
      | Dragging { source = External _; target = Some target; _ } ->
        let target = if target < historical_min_index then Int.max_value else target in
        { adjusted_index = (if index >= target then index + 1 else index)
        ; is_dragged_item
        ; is_target_of_external_item = index = target
        }
  in
  let%sub total_height, alist_for_targets =
    let%arr input = input
    and sizes = sizes in
    let items =
      Array.of_list_map (Map.to_alist input) ~f:(fun (key, (data, index)) ->
        let size =
          match Map.find sizes key with
          | Some size -> Int.of_float size.height
          | None -> default_item_height
        in
        ( key
        , { data
          ; index
          ; y_position = 0
          ; adjusted = 0
          ; size
          ; is_target_of_external_item = false
          ; is_dragged_item = false
          } ))
    in
    Array.sort items ~compare:(fun a b ->
      Comparable.lift Int.compare ~f:(fun (_, { index; _ }) -> index) a b);
    let (total_height : int) =
      Array.fold_map_inplace items ~init:0 ~f:(fun acc (key, item) ->
        acc + item.size, (key, { item with y_position = acc }))
    in
    total_height, Array.to_list items
  in
  let%sub map_for_targets =
    return (alist_for_targets >>| Map.of_alist_exn (module Key))
  in
  let%sub map_for_items =
    let%arr items = alist_for_targets
    and model_info_at_index = model_info_at_index in
    let items = Array.of_list items in
    Array.map_inplace items ~f:(fun (key, item) ->
      let { adjusted_index; is_target_of_external_item; is_dragged_item } =
        model_info_at_index item.index
      in
      ( key
      , { item with
          adjusted = adjusted_index
        ; is_dragged_item
        ; is_target_of_external_item
        } ));
    Array.sort items ~compare:(fun a b ->
      Comparable.lift Int.compare ~f:(fun (_, { adjusted; _ }) -> adjusted) a b);
    Array.fold
      items
      ~init:(0, Map.empty (module Key))
      ~f:(fun (acc, items) (key, item) ->
        let adjusted =
          if item.is_target_of_external_item then acc + default_item_height else acc
        in
        adjusted + item.size, Map.add_exn items ~key ~data:{ item with adjusted })
    |> snd
  in
  let%sub items =
    Bonsai.assoc
      (module Key)
      map_for_items
      ~f:(fun key data ->
        let%sub key_sexp =
          let%arr key = key in
          let key_sexp = Key.sexp_of_t key in
          [%string "source-%{key_sexp#Sexp}"]
        in
        let%sub size_attr = return (size_attr <*> key) in
        let%sub { data; adjusted = y_position; is_dragged_item; _ } = return data in
        let%arr data = data
        and y_position = y_position
        and is_dragged_item = is_dragged_item
        and extra_item_attrs = extra_item_attrs
        and size_attr = size_attr
        and key_sexp = key_sexp in
        Node.div
          ~key:key_sexp
          ~attrs:
            [ Attr.(
                size_attr
                @ transform_xy ~left ~right 0 y_position
                @ extra_item_attrs
                @ if is_dragged_item then Attr.style (Css_gen.opacity 0.0) else Attr.empty)
            ]
          [ data ])
  in
  let%sub targets =
    let%sub num_items =
      let%arr map = input in
      Map.fold ~init:(-1) map ~f:(fun ~key:_ ~data:(_, rank) acc -> Int.max rank acc) + 1
    in
    let%sub drop_target = return (dnd >>| Drag_and_drop.drop_target) in
    let single_target ~is_the_extra_target index size y_position =
      let%sub is_the_extra_target =
        let%arr should_render_extra_target = should_render_extra_target
        and index = index
        and num_items = num_items in
        ((not should_render_extra_target) && index = num_items - 1) || is_the_extra_target
      in
      let%arr index = index
      and size = size
      and y_position = y_position
      and is_the_extra_target = is_the_extra_target
      and drop_target = drop_target in
      let the_height =
        if is_the_extra_target
        then
          Css_gen.create
            ~field:"height"
            ~value:[%string "calc(100% - %{y_position#Int}px)"]
        else Css_gen.height (`Px size)
      in
      Node.div
        ~key:[%string "target-%{index#Int}"]
        ~attrs:
          [ Attr.(
              drop_target ~id:index
              @ transform_xy ~left ~right 0 y_position
              @ style (Css_gen.position ~top:(`Px 0) ~left ~right `Absolute)
              @ (if enable_debug_overlay
                 then
                   style
                     Css_gen.(
                       background_color (`Hex "#10000010") @> border ~style:`Dotted ())
                 else empty)
              @ style the_height)
          ]
        []
    in
    let%sub item_targets =
      Bonsai.assoc
        (module Key)
        map_for_targets
        ~f:(fun _ data ->
          let%sub { index; size; y_position; _ } = return data in
          single_target ~is_the_extra_target:false index size y_position)
    in
    let%sub extra_target =
      if%sub should_render_extra_target
      then
        single_target
          ~is_the_extra_target:true
          num_items
          (Value.return default_item_height)
          total_height
      else Bonsai.const Vdom.Node.none
    in
    let%arr item_targets = item_targets
    and extra_target = extra_target in
    extra_target :: Map.data item_targets
  in
  let%sub is_dragging =
    let%arr dnd = dnd in
    match Drag_and_drop.model dnd with
    | Not_dragging -> false
    | _ -> true
  in
  let%sub item_is_hovered =
    let%arr dnd = dnd in
    match Drag_and_drop.model dnd with
    | Dragging { target = Some _; _ } -> true
    | _ -> false
  in
  let%sub empty_list_placeholder = empty_list_placeholder ~item_is_hovered in
  let%arr items = items
  and targets = targets
  and is_dragging = is_dragging
  and total_height = total_height
  and empty_list_placeholder = empty_list_placeholder in
  let items = if Map.is_empty items then [ empty_list_placeholder ] else Map.data items in
  let items = if is_dragging then items @ targets else items in
  Node.div
    ~attrs:
      [ Attr.(
          style
            Css_gen.(
              position `Relative
              @> flex_container ~direction:`Column ()
              @> height (`Percent (Percent.of_percentage 100.0))
              @> min_height (`Px (total_height + default_item_height))))
      ]
    items
;;

module Reorderable_list (Key : Comparator) : sig
  type t = int Map.M(Key).t [@@deriving sexp, equal]

  val of_list : Key.t list -> t
  val empty : t
  val move : t -> Key.t -> int -> t
  val set : t -> Key.t -> t
  val remove : t -> Key.t -> t
end = struct
  type t = int Map.M(Key).t [@@deriving sexp, equal]

  let of_list sources =
    List.mapi sources ~f:(fun index source -> source, index)
    |> Map.of_alist_exn (module Key)
  ;;

  let empty = Map.empty (module Key)

  let set model source =
    Map.update model source ~f:(function
      | Some index -> index
      | None -> Map.length model)
  ;;

  let move model source target =
    let model = set model source in
    let source = Map.find_exn model source in
    Map.map model ~f:(fun index ->
      if source = index
      then target
      else if source = target
      then index
      else if source < target
      then if source < index && index <= target then index - 1 else index
      else if source > index && index >= target
      then index + 1
      else index)
  ;;

  let remove model source =
    let moved_to_end = move model source (Map.length model) in
    Map.remove moved_to_end source
  ;;
end

module Action = struct
  type 'a item =
    | Move of 'a * int
    | Set of 'a
    | Remove of 'a
    | Overwrite of 'a list
  [@@deriving sexp]

  type 'a t = 'a item list [@@deriving sexp]
end

let with_inject
  (type src cmp)
  (key : (src, cmp) comparator)
  ?(sentinel_name = "dnd")
  ?enable_debug_overlay
  ?extra_item_attrs
  ?left
  ?right
  ?empty_list_placeholder
  ?default_item_height
  render
  =
  let module Key = struct
    include (val key)

    let equal a b = comparator.compare a b = 0
  end
  in
  let module A = struct
    type t = Key.t Action.t [@@deriving sexp_of]
  end
  in
  let module Model = Reorderable_list (Key) in
  let apply_action model (action : Key.t Action.item) =
    match action with
    | Move (source, target) -> Model.move model source target
    | Set source -> Model.set model source
    | Remove source -> Model.remove model source
    | Overwrite sources -> Model.of_list sources
  in
  let%sub ranked_input, inject =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: A.t]
      ~default_model:(Map.empty (module Key))
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model actions ->
        List.fold actions ~init:model ~f:apply_action)
  in
  let%sub dnd =
    Bonsai_web_ui_drag_and_drop.create
      ~source_id:(module Key)
      ~target_id:(module Int)
      ~on_drop:
        (let%map inject = inject in
         fun key i -> inject [ Move (key, i) ])
  in
  let%sub source = return (dnd >>| Bonsai_web_ui_drag_and_drop.source) in
  let%sub rendered_ranked_input =
    Bonsai.assoc
      (module Key)
      ranked_input
      ~f:(fun key data ->
        let%sub source =
          let%arr key = key
          and source = source in
          source ~id:key
        in
        let%sub rendered = render ~index:data ~source key in
        return (Value.both rendered data))
  in
  let%sub input =
    Bonsai.assoc
      (module Key)
      rendered_ranked_input
      ~f:(fun _ data ->
        let%arr (_, view), rank = data in
        view, rank)
  in
  let%sub list =
    list
      (module Key)
      ~dnd
      ?enable_debug_overlay
      ?extra_item_attrs
      ?left
      ?right
      ?empty_list_placeholder
      ?default_item_height
      input
  in
  let%sub sentinel = return (dnd >>| Drag_and_drop.sentinel) in
  let%sub dragged_element =
    Drag_and_drop.dragged_element dnd ~f:(fun key ->
      let%sub result =
        Bonsai.Incr.compute
          (Value.both key rendered_ranked_input)
          ~f:(fun key_and_input ->
          let%pattern_bind.Ui_incr key, rendered_ranked_input = key_and_input in
          let lookup =
            Ui_incr.Map.Lookup.create rendered_ranked_input ~comparator:Key.comparator
          in
          let%bind.Ui_incr key = key in
          Ui_incr.Map.Lookup.find lookup key)
      in
      match%sub result with
      | Some ((_, view), _) -> return view
      | None -> Bonsai.const Vdom.Node.None)
  in
  let%sub view =
    let%arr list = list
    and sentinel = sentinel
    and dragged_element = dragged_element in
    Vdom.Node.div ~attrs:[ sentinel ~name:sentinel_name ] [ list; dragged_element ]
  in
  let%sub ranking =
    let%arr rendered_ranked_input = rendered_ranked_input in
    Map.to_alist rendered_ranked_input
    |> List.sort ~compare:(fun a b ->
         Comparable.lift Int.compare ~f:(fun (_, (_, rank)) -> rank) a b)
    |> List.map ~f:(fun (key, ((extra, _), _)) -> key, extra)
  in
  return (Value.map3 ranking view inject ~f:Tuple3.create)
;;

let sync_with_set
  (type a cmp)
  (module Key : Comparator with type t = a and type comparator_witness = cmp)
  (input : (a, cmp) Set.t Value.t)
  ~inject
  ~add
  ~remove
  =
  let%sub callback =
    let%arr inject = inject
    and add = add
    and remove = remove in
    fun old new_ ->
      inject
        (match old with
         | Some old ->
           Set.symmetric_diff old new_
           |> Sequence.fold
                ~init:Reversed_list.[]
                ~f:(fun acc -> function
                  | Second k -> add k :: acc
                  | First k -> remove k :: acc)
           |> Reversed_list.rev
         | None ->
           Set.fold new_ ~init:Reversed_list.[] ~f:(fun acc key -> add key :: acc)
           |> Reversed_list.rev)
  in
  Bonsai.Edge.on_change' ~equal:[%equal: Set.M(Key).t] input ~callback
;;

let simple
  (type src cmp)
  (key : (src, cmp) comparator)
  ?sentinel_name
  ?enable_debug_overlay
  ?extra_item_attrs
  ?left
  ?right
  ?empty_list_placeholder
  ?default_item_height
  ~render
  (input : (src, cmp) Set.t Value.t)
  =
  let%sub value, view, inject =
    with_inject
      key
      ?sentinel_name
      ?enable_debug_overlay
      ?extra_item_attrs
      ?left
      ?right
      ?empty_list_placeholder
      ?default_item_height
      render
  in
  let module Key = (val key) in
  let%sub () =
    sync_with_set
      (module Key)
      input
      ~inject
      ~add:(Value.return (fun key -> Action.Set key))
      ~remove:(Value.return (fun key -> Action.Remove key))
  in
  return (Value.both value view)
;;

module Multi = struct
  module Multi_reorderable_list (Which : Comparator) (Key : Comparator) : sig
    type t = Reorderable_list(Key).t Map.M(Which).t [@@deriving sexp, equal]

    val of_lists : Key.t list Map.M(Which).t -> t
    val empty : t
    val move : t -> Key.t -> Which.t -> int -> t
    val set : t -> Which.t -> Key.t -> t
    val remove : t -> Key.t -> t
  end = struct
    module Reorderable_list = Reorderable_list (Key)

    type t = Reorderable_list.t Map.M(Which).t [@@deriving sexp, equal]

    let of_lists sources = Map.map sources ~f:Reorderable_list.of_list
    let empty = Map.empty (module Which)

    let update_list_exn t which ~f =
      Map.update t which ~f:(function
        | None -> f Reorderable_list.empty
        | Some list -> f list)
    ;;

    let move t source target_which target =
      let t = Map.map t ~f:(fun list -> Reorderable_list.remove list source) in
      update_list_exn t target_which ~f:(fun list ->
        Reorderable_list.move list source target)
    ;;

    let set model which key =
      Map.update model which ~f:(fun list ->
        let list = Option.value ~default:Reorderable_list.empty list in
        Reorderable_list.set list key)
    ;;

    let remove t key = Map.map t ~f:(fun list -> Reorderable_list.remove list key)
  end

  module Action = struct
    type ('key, 'which, 'which_cmp) item =
      | Move of 'key * 'which * int
      | Set of 'which * 'key
      | Remove of 'key
      | Overwrite of ('which, 'key list, 'which_cmp) Map.t

    type ('key, 'which, 'which_cmp) t = ('key, 'which, 'which_cmp) item list
  end

  let with_inject
    (type src cmp which which_cmp)
    (key : (src, cmp) comparator)
    (which : (which, which_cmp) comparator)
    ?(sentinel_name = "dnd")
    ?enable_debug_overlay
    ?extra_item_attrs
    ?left
    ?right
    ?empty_list_placeholder
    ?default_item_height
    ~(lists : (which, which_cmp) Set.t Value.t)
    (render :
      index:int Value.t
      -> source:Vdom.Attr.t Value.t
      -> which Value.t
      -> src Value.t
      -> (_ * Vdom.Node.t) Computation.t)
    =
    let module Key = struct
      include (val key)

      let equal a b = comparator.compare a b = 0
    end
    in
    let module Which = struct
      include (val which)

      let equal a b = comparator.compare a b = 0
    end
    in
    let module Action = struct
      type item = (Key.t, Which.t, Which.comparator_witness) Action.item

      let sexp_of_t = sexp_of_opaque
    end
    in
    let module Model = Multi_reorderable_list (Which) (Key) in
    let apply_action model (action : Action.item) =
      match action with
      | Move (source, target_which, target) -> Model.move model source target_which target
      | Set (which, source) -> Model.set model which source
      | Remove source -> Model.remove model source
      | Overwrite sources -> Model.of_lists sources
    in
    let%sub ranked_input, inject =
      Bonsai.state_machine0
        ()
        ~sexp_of_action:[%sexp_of: Action.t]
        ~equal:[%equal: Model.t]
        ~default_model:Model.empty
        ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model actions ->
        List.fold actions ~init:model ~f:apply_action)
    in
    let%sub dnd =
      Bonsai_web_ui_drag_and_drop.create
        ~source_id:(module Key)
        ~target_id:
          (module struct
            type t = Which.t * Int.t [@@deriving equal, sexp]
          end)
        ~on_drop:
          (let%map inject = inject in
           fun source (target_which, target) ->
             inject [ Move (source, target_which, target) ])
    in
    let%sub source = return (dnd >>| Bonsai_web_ui_drag_and_drop.source) in
    let%sub rendered_ranked_input =
      let%sub ranked_input =
        Bonsai.Incr.compute ranked_input ~f:(fun ranked_input ->
          Incr_map.collapse ~comparator:(module Key) ranked_input
          |> Incr_map.mapi ~f:(fun ~key:(which, _key) ~data -> which, data)
          |> Incr_map.rekey
               ~comparator:(module Key)
               ~f:(fun ~key:(_which, key) ~data:_ -> key))
      in
      Bonsai.assoc
        (module Key)
        ranked_input
        ~f:(fun key data ->
          let%sub source =
            let%arr key = key
            and source = source in
            source ~id:key
          in
          let%sub which, index = return data in
          let%sub rendered = render ~index ~source which key in
          return (Value.both rendered data))
    in
    let%sub results =
      Bonsai.assoc_set
        (module Which)
        lists
        ~f:(fun which ->
          let%sub rendered_ranked_input =
            Bonsai.Incr.compute
              (Value.both which rendered_ranked_input)
              ~f:(fun which_and_map ->
              let%pattern_bind.Incr which, map = which_and_map in
              (* NOTE: This [bind] only runs once (The key (which) from [assoc_set] does
                   not change). *)
              let%bind.Incr current_which = which in
              Incr_map.filter_map map ~f:(fun (data, (which, index)) ->
                match Which.equal which current_which with
                | false -> None
                | true -> Some (data, index)))
          in
          let%sub input =
            Bonsai.assoc
              (module Key)
              rendered_ranked_input
              ~f:(fun _ data ->
                let%arr (_, view), rank = data in
                view, rank)
          in
          let%sub dnd =
            let%arr dnd = dnd
            and which = which in
            Drag_and_drop.project_target
              dnd
              ~map:(fun (target_which, index) ->
                if Which.equal target_which which then index else -1)
              ~unmap:(fun index -> which, index)
          in
          let%sub view =
            list
              (module Key)
              ~dnd
              ?enable_debug_overlay
              ?extra_item_attrs
              ?left
              ?right
              ?empty_list_placeholder:
                (Option.map empty_list_placeholder ~f:(fun f ~item_is_hovered ->
                   f ~item_is_hovered which))
              ?default_item_height
              input
          in
          let%sub value =
            let%arr rendered_ranked_input = rendered_ranked_input in
            Map.to_alist rendered_ranked_input
            |> List.sort ~compare:(fun a b ->
                 Comparable.lift Int.compare ~f:(fun (_, (_, rank)) -> rank) a b)
            |> List.map ~f:(fun (key, ((extra, _), _)) -> key, extra)
          in
          return (Value.map3 value view rendered_ranked_input ~f:Tuple3.create))
    in
    let%sub sentinel = return (dnd >>| Drag_and_drop.sentinel) in
    let%sub dragged_element =
      Drag_and_drop.dragged_element dnd ~f:(fun target ->
        let%sub result =
          Bonsai.Incr.compute (Value.both target results) ~f:(fun target_and_results ->
            let%pattern_bind.Ui_incr source, results = target_and_results in
            let%map.Ui_incr source = source
            and results = results in
            List.find_map (Map.to_alist results) ~f:(fun (_, (_, _, list)) ->
              Map.find list source))
        in
        match%sub result with
        | Some ((_, view), _) -> return view
        | None -> Bonsai.const Vdom.Node.None)
    in
    let%sub results =
      Bonsai.assoc
        (module Which)
        results
        ~f:(fun _key data ->
          let%arr value, view, _ = data in
          value, view)
    in
    let%arr results = results
    and sentinel = sentinel
    and dragged_element = dragged_element
    and inject = inject in
    let view =
      Vdom.Node.div ~attrs:[ sentinel ~name:sentinel_name ] [ dragged_element ]
    in
    results, view, inject
  ;;

  let simple
    (type src cmp which which_cmp)
    (key : (src, cmp) comparator)
    (which : (which, which_cmp) comparator)
    ?sentinel_name
    ?enable_debug_overlay
    ?extra_item_attrs
    ?left
    ?right
    ?empty_list_placeholder
    ?default_item_height
    ~(render :
        index:int Value.t
        -> source:Vdom.Attr.t Value.t
        -> which Value.t
        -> src Value.t
        -> (_ * Vdom.Node.t) Computation.t)
    ~(lists : (which, which_cmp) Set.t Value.t)
    ~default_list
    (input : (src, cmp) Set.t Value.t)
    =
    let%sub value, view, inject =
      with_inject
        key
        which
        ?sentinel_name
        ?enable_debug_overlay
        ?extra_item_attrs
        ?left
        ?right
        ?empty_list_placeholder
        ?default_item_height
        ~lists
        render
    in
    let%sub () =
      let%sub add =
        let%arr default_list = default_list in
        fun k -> Action.Set (default_list, k)
      in
      sync_with_set
        key
        input
        ~inject
        ~add
        ~remove:(Value.return (fun key -> Action.Remove key))
    in
    return (Value.both value view)
  ;;
end
