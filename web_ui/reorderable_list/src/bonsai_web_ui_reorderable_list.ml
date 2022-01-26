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
      (key : (src, cmp) Bonsai.comparator)
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
    let%arr model = model in
    fun index ->
      let is_dragged_item =
        match model with
        | Dragging { source = In_list source; _ } -> source = index
        | Not_dragging | Dragging { source = External _; _ } -> false
      in
      match model with
      | Not_dragging | Dragging { target = None; _ } ->
        { adjusted_index = index; is_target_of_external_item = false; is_dragged_item }
      | Dragging { source = In_list source; target = Some target; _ } ->
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
    Array.sort items ~compare:(fun (_, { index = a; _ }) (_, { index = b; _ }) ->
      Int.compare a b);
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
    Array.sort items ~compare:(fun (_, { adjusted = a; _ }) (_, { adjusted = b; _ }) ->
      Int.compare a b);
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
          ~attr:
            Attr.(
              size_attr
              @ transform_xy ~left ~right 0 y_position
              @ extra_item_attrs
              @ if is_dragged_item then Attr.style (Css_gen.opacity 0.0) else Attr.empty)
          [ data ])
  in
  let%sub targets =
    let%sub num_items = return (input >>| Map.length) in
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
        ~attr:
          Attr.(
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
    ~attr:
      Attr.(
        style
          Css_gen.(
            position `Relative
            @> flex_container ~direction:`Column ()
            @> height (`Percent (Percent.of_percentage 100.0))
            @> min_height (`Px (total_height + default_item_height))))
    items
;;

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
      (key : (src, cmp) Bonsai.comparator)
      ?(sentinel_name = "dnd")
      ?enable_debug_overlay
      ?extra_item_attrs
      ?left
      ?right
      ?empty_list_placeholder
      ?default_item_height
      render
  =
  let module Key = (val key) in
  let module A = struct
    type t = Key.t Action.t [@@deriving sexp]
  end
  in
  let module Model = struct
    type t = int Map.M(Key).t [@@deriving sexp, equal]
  end
  in
  let move model source target =
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
  in
  let apply_action model (action : Key.t Action.item) =
    match action with
    | Move (source, target) -> move model source target
    | Set source ->
      Map.update model source ~f:(function
        | Some index -> index
        | None -> Map.length model)
    | Remove source ->
      let moved_to_end = move model source (Map.length model) in
      Map.remove moved_to_end source
    | Overwrite sources ->
      List.mapi sources ~f:(fun index source -> source, index)
      |> Map.of_alist_exn (module Key)
  in
  let%sub ranked_input, inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module A)
      ~default_model:(Map.empty (module Key))
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model actions ->
        List.fold actions ~init:model ~f:apply_action)
  in
  let%sub dnd =
    Bonsai_web_ui_drag_and_drop.create
      [%here]
      ~source_id:
        (module struct
          type t = Key.t [@@deriving sexp]

          let equal a b = Key.comparator.compare a b = 0
        end)
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
    Vdom.Node.div ~attr:(sentinel ~name:sentinel_name) [ list; dragged_element ]
  in
  let%sub ranking =
    let%arr rendered_ranked_input = rendered_ranked_input in
    Map.to_alist rendered_ranked_input
    |> List.sort ~compare:(fun (_, (_, a)) (_, (_, b)) -> Int.compare a b)
    |> List.map ~f:(fun (key, ((extra, _), _)) -> key, extra)
  in
  return (Value.map3 ranking view inject ~f:Tuple3.create)
;;

let simple
      (type src cmp)
      (key : (src, cmp) Bonsai.comparator)
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
    Bonsai.Edge.on_change'
      [%here]
      (module struct
        type t = Set.M(Key).t [@@deriving sexp, equal]
      end)
      input
      ~callback:
        (let%map inject = inject in
         fun old new_ ->
           inject
             (match old with
              | Some old ->
                Set.symmetric_diff old new_
                |> Sequence.fold
                     ~init:Reversed_list.[]
                     ~f:
                       (fun acc -> function
                          | Second k -> Action.Set k :: acc
                          | First k -> Remove k :: acc)
                |> Reversed_list.rev
              | None ->
                Set.fold
                  new_
                  ~init:Reversed_list.[]
                  ~f:(fun acc key -> Action.Set key :: acc)
                |> Reversed_list.rev))
  in
  return (Value.both value view)
;;
