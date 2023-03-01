open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Vdom

module Model = struct
  type 'k suggestion_list_state =
    | Closed
    | First_item
    | Selected of 'k
  [@@deriving equal, sexp, compare]

  type 'k t =
    { query : string
    ; suggestion_list_state : 'k suggestion_list_state
    ; offset : int
    }
  [@@deriving equal, sexp]
end

module Action = struct
  type 'k t =
    | Set_query of string
    | Move_next
    | Move_prev
    | Move_to of
        { key : 'k
        ; offset : int
        }
    | Move_next_with_fixed_offset
    | Move_prev_with_fixed_offset
    | Close_suggestions
    | Open_suggestions
  [@@deriving sexp]
end

module Suggestion_list_kind = struct
  type t =
    | Transient_overlay
    | Permanent_fixture
  [@@deriving sexp, compare, enumerate, equal]
end

module Expand_direction = struct
  type t =
    | Down
    | Up
  [@@deriving sexp, compare, enumerate, equal]
end

let select_key ~first_try ~then_try ~else_use =
  match first_try with
  | Some (key, _) -> Model.Selected key
  | None ->
    (match then_try with
     | (lazy (Some (key, _))) -> Model.Selected key
     | (lazy None) -> else_use)
;;

type 'k t =
  { selected_item : 'k option
  ; view : Vdom.Node.t
  ; query : string
  }
[@@deriving fields]

let create
      (type k cmp)
      (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
      ?(initial_query = "")
      ?(max_visible_items = Value.return 10)
      ?(suggestion_list_kind = Value.return Suggestion_list_kind.Transient_overlay)
      ?(expand_direction = Value.return Expand_direction.Down)
      ?(selected_item_attr = Value.return Attr.empty)
      ?(extra_list_container_attr = Value.return Attr.empty)
      ?(extra_input_attr = Value.return Attr.empty)
      ?(extra_attr = Value.return Attr.empty)
      ~f
      ~on_select
      ()
  =
  let%sub { Model.query; suggestion_list_state; offset }, inject, items, _ =
    Bonsai.wrap
      (module struct
        type t = Key.t Model.t [@@deriving sexp]

        let equal a b = Model.equal (fun a b -> Key.comparator.compare a b = 0) a b
      end)
      ~default_model:
        { Model.query = initial_query; suggestion_list_state = Closed; offset = 0 }
      ~apply_action:
        (fun ~inject:_ ~schedule_event:_ (_, _, items, max_visible_items) model action ->
           let suggestion_list_state =
             (* We normalize which item is selected in case the list has changed
                since the last action. Normalization just means setting the
                selected key to the closest thing that actually exists. *)
             match model.suggestion_list_state with
             | Selected key ->
               select_key
                 ~first_try:(Map.closest_key items `Less_or_equal_to key)
                 ~then_try:(lazy (Map.closest_key items `Greater_or_equal_to key))
                 ~else_use:First_item
             | First_item -> First_item
             | Closed -> Closed
           in
           let next_suggestion_list_state () =
             match suggestion_list_state with
             | Selected key ->
               select_key
                 ~first_try:(Map.closest_key items `Greater_than key)
                 ~then_try:(lazy (Map.min_elt items))
                 ~else_use:(Selected key)
             | First_item ->
               (match Map.min_elt items with
                | None -> First_item
                | Some (first_key, _) ->
                  (match Map.closest_key items `Greater_than first_key with
                   | None -> Selected first_key
                   | Some (second_key, _) -> Selected second_key))
             | Closed -> First_item
           in
           let prev_suggestion_list_state () =
             match model.suggestion_list_state with
             | Selected key ->
               select_key
                 ~first_try:(Map.closest_key items `Less_than key)
                 ~then_try:(lazy (Map.max_elt items))
                 ~else_use:(Selected key)
             | First_item | Closed ->
               (match Map.max_elt items with
                | None -> First_item
                | Some (last_key, _) -> Selected last_key)
           in
           match action with
           | Action.Set_query query ->
             let suggestion_list_state =
               match suggestion_list_state with
               | Selected key -> Model.Selected key
               | First_item | Closed -> First_item
             in
             let offset = model.offset in
             { Model.query; suggestion_list_state; offset }
           | Open_suggestions -> { model with suggestion_list_state = First_item }
           | Close_suggestions -> { model with suggestion_list_state = Closed }
           | Move_next ->
             let suggestion_list_state = next_suggestion_list_state () in
             let offset =
               let comparison =
                 Model.compare_suggestion_list_state
                   (Map.comparator items).compare
                   model.suggestion_list_state
                   suggestion_list_state
               in
               if comparison = 0
               then model.offset
               else if comparison < 0
               then min (max_visible_items - 1) (model.offset + 1)
               else 0
             in
             { model with suggestion_list_state; offset }
           | Move_prev ->
             let suggestion_list_state = prev_suggestion_list_state () in
             let offset =
               let comparison =
                 Model.compare_suggestion_list_state
                   (Map.comparator items).compare
                   model.suggestion_list_state
                   suggestion_list_state
               in
               if comparison = 0
               then model.offset
               else if comparison < 0
               then max_visible_items - 1
               else max 0 (model.offset - 1)
             in
             { model with suggestion_list_state; offset }
           | Move_to { key; offset } ->
             if Map.mem items key
             then { model with suggestion_list_state = Selected key; offset }
             else model
           | Move_next_with_fixed_offset ->
             { model with suggestion_list_state = next_suggestion_list_state () }
           | Move_prev_with_fixed_offset ->
             { model with suggestion_list_state = prev_suggestion_list_state () })
      ~f:(fun model inject ->
        let%sub { Model.query; _ } = return model in
        let%sub items = f query in
        let%arr model = model
        and inject = inject
        and items = items
        and max_visible_items = max_visible_items in
        model, inject, items, max_visible_items)
  in
  let%sub selected_key =
    match%sub suggestion_list_state with
    | Selected key ->
      let%arr key = key
      and items = items in
      (match Map.closest_key items `Less_or_equal_to key with
       | Some (key, _) -> Some key
       | None ->
         (match Map.closest_key items `Greater_or_equal_to key with
          | Some (key, _) -> Some key
          | None -> None))
    | First_item ->
      let%arr items = items in
      (match Map.min_elt items with
       | Some (key, _) -> Some key
       | None -> None)
    | Closed -> Bonsai.const None
  in
  let%sub items =
    let%arr items = items
    and max_visible_items = max_visible_items
    and selected_key = selected_key
    and offset = offset in
    match selected_key with
    | Some selected_key ->
      let length = ref 0 in
      let items = ref items in
      let result = ref (Map.empty (module Key)) in
      (* We alternate between taking something larger and smaller than the
         selected key until we have taken [max_visible_items] or have exhausted
         the source list. This is probably not done in the most efficient
         manner, but it's O(max_visible_items * log(number_of_items)), which is
         probably acceptable if [max_visible_items] is small. *)
      let visible_items = min max_visible_items (Map.length !items) in
      let offset = min offset visible_items in
      let add_element_from_side side =
        match Map.closest_key !items side selected_key with
        | Some (key, data) ->
          result := Map.set !result ~key ~data;
          items := Map.remove !items key;
          incr length;
          true
        | None -> false
      in
      while !length < visible_items do
        (* An invariant of this loop is that we will always increment [length]
           every iteration, thus guaranteeing the termination of the loop. This
           is guaranteed because [visible_items] is defined to be at most the
           length of the map of items being pulled from, which ensures that we
           won't run out of items collect until we have met the desired length. *)
        if !length <= offset
        then (
          if not (add_element_from_side `Less_or_equal_to)
          then ignore (add_element_from_side `Greater_or_equal_to : bool))
        else if not (add_element_from_side `Greater_or_equal_to)
        then ignore (add_element_from_side `Less_or_equal_to : bool)
      done;
      !result
    | None ->
      Map.subrange
        items
        ~lower_bound:Unbounded
        ~upper_bound:
          (match Map.nth items max_visible_items with
           | Some (k, _) -> Excl k
           | None -> Unbounded)
  in
  let%sub get_items = Bonsai.yoink items in
  let%sub items =
    Bonsai.assoc
      (module Key)
      items
      ~f:(fun key item ->
        let%arr key = key
        and item = item
        and get_items = get_items
        and selected_key = selected_key
        and selected_item_attr = selected_item_attr
        and inject = inject
        and on_select = on_select in
        let selected_attr =
          match selected_key with
          | Some selected_key when Key.comparator.compare key selected_key = 0 ->
            selected_item_attr
          | _ -> Attr.empty
        in
        let move_to_effect =
          let%bind.Effect items =
            match%bind.Effect get_items with
            | Active items -> Effect.return items
            | Inactive ->
              Effect.never
          in
          let%bind.Effect offset = Effect.of_sync_fun (Map.rank items) key in
          let offset = Option.value offset ~default:0 in
          inject (Move_to { key; offset })
        in
        let attr =
          Attr.many
            [ selected_attr
            ; Attr.on_mouseenter (fun _ -> move_to_effect)
            ; Attr.on_click (fun _ ->
                let%bind.Effect () = inject (Set_query "") in
                let%bind.Effect () = inject Close_suggestions in
                on_select key)
            ]
        in
        Node.div ~attr [ item ])
  in
  let%sub handle_keydown =
    let%arr inject = inject
    and selected_key = selected_key
    and on_select = on_select
    and expand_direction = expand_direction in
    let open Vdom in
    let open Js_of_ocaml in
    fun ev ->
      let move_next = Effect.Many [ inject Move_next; Effect.Prevent_default ] in
      let move_prev = Effect.Many [ inject Move_prev; Effect.Prevent_default ] in
      let up, down =
        match expand_direction with
        | Up -> move_next, move_prev
        | Down -> move_prev, move_next
      in
      match Dom_html.Keyboard_code.of_event ev with
      | ArrowUp -> up
      | Tab when Js.to_bool ev##.shiftKey ->
        (match selected_key with
         | Some _ -> up
         | None -> Effect.Ignore)
      | ArrowDown -> down
      | Tab ->
        (match selected_key with
         | Some _ -> down
         | None -> Effect.Ignore)
      | Escape -> inject Action.Close_suggestions
      | Enter ->
        (match selected_key with
         | Some key ->
           Effect.Many
             [ on_select key
             ; inject (Set_query "")
             ; inject Close_suggestions
             ; Effect.Prevent_default
             ]
         | None -> inject Open_suggestions)
      | _ -> Effect.Ignore
  in
  let%sub suggestion_container_id = Bonsai.path_id in
  let%sub input_id = Bonsai.path_id in
  let%arr query = query
  and selected_key = selected_key
  and inject = inject
  and handle_keydown = handle_keydown
  and suggestion_list_kind = suggestion_list_kind
  and expand_direction = expand_direction
  and items = items
  and extra_list_container_attr = extra_list_container_attr
  and extra_input_attr = extra_input_attr
  and extra_attr = extra_attr
  and suggestion_container_id = suggestion_container_id
  and input_id = input_id in
  let container_position, suggestions_position, is_open =
    match suggestion_list_kind with
    | Suggestion_list_kind.Transient_overlay ->
      let is_open = Option.is_some selected_key in
      ( Attr.style (Css_gen.position `Relative)
      , Attr.style (Css_gen.position `Absolute)
      , is_open )
    | Permanent_fixture -> Attr.empty, Attr.empty, true
  in
  let on_blur =
    Attr.on_blur
      (let open Js_of_ocaml in
       fun (ev : Dom_html.focusEvent Js.t) ->
         (* Blurring usually means that we want to close the suggestion list.
            However, if we are blurring the text input in order to focus the
            list of items (or vice versa), we want to keep the list open. Thus,
            we check whether the relatedTarget of the event is one of those two
            elements, in which case we don't close the list. *)
         match
           Option.bind (Js.Optdef.to_option ev##.relatedTarget) ~f:Js.Opt.to_option
         with
         | Some related_target ->
           let id = Js.to_string related_target##.id in
           if String.equal id suggestion_container_id || String.equal id input_id
           then Effect.Ignore
           else inject Close_suggestions
         | None -> inject Close_suggestions)
  in
  let input =
    Node.input
      ~attr:
        (Attr.many
           [ Attr.id input_id
           ; Attr.type_ "text"
           ; Attr.string_property "value" query
           ; Attr.on_keydown handle_keydown
           ; Attr.on_input (fun _ query -> inject (Set_query query))
           ; Attr.on_focus (fun _ -> inject Open_suggestions)
           ; on_blur
           ; extra_input_attr
           ])
      ()
  in
  let suggestions =
    match is_open with
    | false -> Node.div []
    | true ->
      let position_above_or_below, directed_items =
        match expand_direction with
        | Up -> Attr.style (Css_gen.bottom (`Px 0)), items |> Map.data |> List.rev
        | Down -> Attr.empty, Map.data items
      in
      let attr =
        Attr.(suggestions_position @ position_above_or_below @ extra_list_container_attr)
      in
      Node.div ~attr directed_items
  in
  let suggestions_container =
    Node.div
      ~attr:
        (Attr.many
           [ Attr.create "data-test" "query-box-item-container"
           ; Attr.id suggestion_container_id
           ; Attr.tabindex (-1)
           ; Attr.on_wheel
               (let open Js_of_ocaml in
                fun (ev : Js_of_ocaml_patches.Dom_html.wheelEvent Js.t) ->
                  let comparison =
                    match expand_direction with
                    | Down -> Float.( < ) ev##.deltaY 0.0
                    | Up -> Float.( > ) ev##.deltaY 0.0
                  in
                  Effect.Many
                    [ (if comparison
                       then inject Move_prev_with_fixed_offset
                       else inject Move_next_with_fixed_offset)
                    ; Effect.Prevent_default
                    ])
           ; on_blur
           ; container_position
           ])
      [ suggestions ]
  in
  let view =
    Node.div
      ~attr:extra_attr
      (match expand_direction with
       | Up -> [ suggestions_container; input ]
       | Down -> [ input; suggestions_container ])
  in
  { selected_item = selected_key; view; query }
;;

let stringable
      (type k cmp)
      (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
      ?initial_query
      ?max_visible_items
      ?suggestion_list_kind
      ?expand_direction
      ?selected_item_attr
      ?extra_list_container_attr
      ?extra_input_attr
      ?extra_attr
      ?(to_view = fun _ string -> Vdom.Node.text string)
      ~on_select
      input
  =
  create
    (module Key)
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?expand_direction
    ?selected_item_attr
    ?extra_list_container_attr
    ?extra_input_attr
    ?extra_attr
    ~on_select
    ~f:(fun query ->
      Bonsai.Incr.compute (Value.both query input) ~f:(fun incr ->
        let%pattern_bind.Incr query, input = incr in
        Incr_map.filter_mapi' input ~f:(fun ~key ~data:string ->
          let%map.Incr string = string
          and query = query in
          if Fuzzy_match.is_match
               ~char_equal:Char.Caseless.equal
               ~pattern:query
               string
          then Some (to_view key string)
          else None)))
    ()
;;

module Collate_map_with_score = struct
  module Scored_key = struct
    module T = struct
      type 'k t = int * 'k

      let sexp_of_t (x, _) = sexp_of_int x
      let t_of_sexp _ = assert false
      let compare (a, _) (b, _) = Int.compare a b
    end

    include T
    include Comparator.Make1 (T)

    module M (T : T) = struct
      type nonrec t = T.t t
      type nonrec comparator_witness = comparator_witness

      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
      let comparator = comparator
    end

    module Map = struct
      type nonrec ('k, 'v) t = ('k t, 'v, comparator_witness) Map.t
    end
  end

  let collate (type k) ~preprocess ~score ~query_is_as_strict ~to_result input query =
    let empty_result =
      Map.empty
        (module Scored_key.M (struct
             type t = k
           end))
    in
    Bonsai.Incr.compute (Value.both input query) ~f:(fun input_and_query ->
      let%pattern_bind.Ui_incr input, query = input_and_query in
      let%bind.Ui_incr input = input in
      let len = Map.length input in
      let array = Uniform_array.unsafe_create_uninitialized ~len in
      let () =
        let index = ref 0 in
        Map.iteri input ~f:(fun ~key ~data ->
          Uniform_array.set array !index (key, data, preprocess ~key ~data);
          incr index)
      in
      (* We keep track of an arbitrary number of queries. Each
         time the query changes, we discard any queries for which the new query
         is not merely a refinement of. In other words, we maintain the
         invariant that each item in this list of queries is strictly more
         general than the previous one. *)
      let previous_queries = ref [] in
      (* In addition, we also keep track of the index (from the back of the
         list of queries, rather than the front, but this doesn't matter
         because we never use the index to get an element out of the list)
         of the first query that eliminated an item from the set of result. *)
      let filtered_out_at_index = Array.create ~len Int.max_value in
      let%map.Ui_incr query = query in
      let rec trim_queries qs =
        match qs with
        | [] -> []
        | q :: qs ->
          if query_is_as_strict query ~as_:q then q :: qs else trim_queries qs
      in
      previous_queries := query :: trim_queries !previous_queries;
      let num_queries = List.length !previous_queries in
      Uniform_array.foldi
        array
        ~init:empty_result
        ~f:(fun index acc (key, data, preprocessed) ->
          let score =
            (* If the item was already filtered out by a previous query, we can
               keep filtering it out. If instead it was filtered out by a query
               that have since discarded (or, possibly, it was never filtered
               out), then we need to re-evaluate the score. *)
            if filtered_out_at_index.(index) < num_queries
            then 0
            else (
              let score = score query preprocessed in
              filtered_out_at_index.(index)
              <- (if score = 0 then num_queries else Int.max_value);
              score)
          in
          if score = 0
          then acc
          else (
            (* The first component of the key compares equivalently to the pair
               (score, index), but faster, since it is only an integer. Note
               that the map comparator doesn't need to inspect the key itself,
               since [index] already captures that ordering. Thus, this whole
               computation remains fast even if the input map comparator is
               extremely slow. *)
            let new_key = (len * score) + index, key in
            Map.add_exn acc ~key:new_key ~data:(to_result preprocessed ~key ~data))))
  ;;
end
