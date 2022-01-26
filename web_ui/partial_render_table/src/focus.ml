open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Collated = Incr_map_collate.Collated

module By_row = struct
  type 'k t =
    { focused : 'k option
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> unit Effect.t
    }
  [@@deriving fields]
end

module Kind = struct
  type ('a, 'k) t =
    | None : (unit, 'k) t
    | By_row : { on_change : ('k option -> unit Effect.t) Value.t } -> ('k By_row.t, 'k) t
end

module Scroll = struct
  (* Top scrolls to the first row in the table
     Bottom scrolls to the last row in the table
     To (id, `Minimal) scrolls to [id], moving the screen as little as possible.
     To (id, `To_top) scrolls the table such that [id] is at the top of the screen.
     To (id, `To_bottom) scrolls the table such that [id] is at the bottom of the screen.  *)
  type t = To of Int63.t * [ `Minimal | `To_top | `To_bottom ]

  let control_test = function
    | To (i, `Minimal) ->
      Effect.print_s [%message "scrolling to" (i : Int63.t) "minimizing scrolling"]
    | To (i, `To_bottom) ->
      Effect.print_s
        [%message
          "scrolling to"
            (i : Int63.t)
            "such that it is positioned at the bottom of the screen"]
    | To (i, `To_top) ->
      Effect.print_s
        [%message
          "scrolling to"
            (i : Int63.t)
            "such that it is positioned at the top of the screen"]
  ;;

  let scroll_into_view_options align smooth =
    let open Js_of_ocaml in
    (* Constructs the options object described here:
       https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView *)
    object%js
      val behavior = Js.string smooth

      val block =
        match align with
        | `Top -> Js.string "start"
        | `Bottom -> Js.string "end"
    end
  ;;

  let the_effect =
    let open Js_of_ocaml in
    Effect.of_sync_fun (fun (row_id, kind, path) ->
      let selector =
        [%string ".partial-render-table-%{path} [data-row-id=\"key_%{row_id#Int63}\"]"]
      in
      Js.Opt.iter
        (Dom_html.document##querySelector (Js.string selector))
        (fun element ->
           let scrollable =
             (Js.Unsafe.coerce element
              : < scrollIntoViewIfNeeded : bool Js.t -> unit Js.meth
             ; scrollIntoView : Js.Unsafe.any -> unit Js.meth >
                                                 Js.t)
           in
           match kind with
           | `Minimal -> scrollable##scrollIntoViewIfNeeded (Js.bool false)
           | `To_bottom ->
             scrollable##scrollIntoView
               (Js.Unsafe.inject (scroll_into_view_options `Bottom "smooth"))
           | `To_top ->
             scrollable##scrollIntoView
               (Js.Unsafe.inject (scroll_into_view_options `Top "smooth"))))
  ;;

  let control_browser path (To (i, k)) = the_effect (i, k, path)

  let control path =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_benchmark -> control_browser path
    | `Node_test -> control_test
    | `Node | `Node_benchmark -> fun _ -> Effect.Ignore
  ;;
end

module Row_machine = struct
  module Triple = struct
    (** This type is pretty integral to the row selection state-machine.  A
        value of this type is stored as the "currently selected row" and also
        as the result for "next row down" queries.  *)
    type 'k t =
      { key : 'k
      ; id : Int63.t
      ; index : int
      }
    [@@deriving sexp, equal]
  end

  module Range_response = struct
    type 'k t =
      | Yes
      | No_but_this_one_is of 'k Triple.t
      | Indeterminate
  end

  let collated_range collated =
    ( Collated.num_before_range collated
    , Collated.num_before_range collated + Collated.length collated )
  ;;

  let find_by collated ~f =
    with_return_option (fun { return } ->
      let i = ref (Collated.num_before_range collated) in
      collated
      |> Collated.to_map_list
      |> Map.iteri ~f:(fun ~key:id ~data:(key, _) ->
        if f ~key ~id ~index:!i
        then return { Triple.key; id; index = !i }
        else Int.incr i))
  ;;

  let find_by_key collated ~key:needle ~key_equal =
    find_by collated ~f:(fun ~key ~id:_ ~index:_ -> key_equal key needle)
  ;;

  let find_by_id collated ~id:needle =
    let map = Collated.to_map_list collated in
    let r =
      match Map.find map needle, Map.rank map needle with
      | Some (key, _), Some rank ->
        let index = Collated.num_before_range collated + rank in
        Some { Triple.key; id = needle; index }
      | _ -> None
    in
    r
  ;;

  let find_by_index collated ~index:needle =
    with_return (fun { return } ->
      find_by collated ~f:(fun ~key:_ ~id:_ ~index ->
        if index > needle then return None;
        Int.equal index needle))
  ;;

  let first_some list =
    List.fold_until
      list
      ~init:()
      ~f:(fun () opt ->
        match Lazy.force opt with
        | Some a -> Stop (Some a)
        | None -> Continue ())
      ~finish:(fun () -> None)
  ;;

  let fetch_or_fail collated ~index =
    match find_by_index collated ~index with
    | Some { Triple.key; id; index } ->
      Range_response.No_but_this_one_is { key; id; index }
    | None ->
      eprint_s [%message [%here] "should be in range"];
      Indeterminate
  ;;

  (* Lookup for the value will first attempt to find the value in this order:
     - lookup by key
     - lookup by index
     - lookup by id

     This function is potentially quite slow, but 99% of the time, there will
     be a hit on the first lookup, which is O(log n). *)
  let find_in_range ~range:(range_start, range_end) ~collated ~key ~id ~index ~key_equal =
    let col_start, col_end = collated_range collated in
    if col_start > range_end || col_end < col_start
    then Range_response.Indeterminate
    else (
      let attempt =
        first_some
          [ lazy (find_by_key collated ~key ~key_equal)
          ; lazy (find_by_index collated ~index)
          ; lazy (find_by_id collated ~id)
          ]
      in
      match attempt with
      | Some ({ Triple.key = _; id = _; index } as triple) ->
        if index >= range_start && index <= range_end
        then Yes
        else if index < range_start
        then fetch_or_fail collated ~index:range_start
        else if index > range_end
        then fetch_or_fail collated ~index:range_end
        else (
          eprint_s [%message [%here] "unreachable" (triple : opaque Triple.t)];
          Indeterminate)
      | None ->
        (match find_by_index collated ~index:range_start with
         | Some r -> No_but_this_one_is r
         | None -> Indeterminate))
  ;;

  module Action = struct
    type 'key t =
      | Unfocus
      | Up
      | Down
      | Page_up
      | Page_down
      | Recompute
      | Select of 'key
    [@@deriving sexp_of]
  end

  let component
        (type key data cmp)
        (key : (key, cmp) Bonsai.comparator)
        ~(on_change : (key option -> unit Effect.t) Value.t)
        ~(collated : (key, data) Incr_map_collate.Collated.t Value.t)
        ~(rows_covered_by_header : int Value.t)
        ~(range : (int * int) Value.t)
        ~(remapped : (key, Int63.t * data, cmp) Map.t Value.t)
        ~path
    : key By_row.t Computation.t
    =
    let module Key = struct
      include (val key)

      let equal a b = comparator.compare a b = 0
    end
    in
    let module Action = struct
      include Action

      type t = Key.t Action.t [@@deriving sexp_of]
    end
    in
    let module Model = struct
      (** [current] is the currently selected row.
          [shadow] is the previously selected row.

          Shadow is useful for computing "next row down" if the user previously
          unfocused, or if the element that was previously selected has been
          removed. *)
      type t =
        { shadow : Key.t Triple.t option
        ; current : Key.t Triple.t option
        }
      [@@deriving sexp, equal]

      let empty = { shadow = None; current = None }
    end
    in
    let%sub input =
      let%arr collated = collated
      and path = path
      and range = range
      and rows_covered_by_header = rows_covered_by_header
      and on_change = on_change in
      collated, range, Scroll.control path, rows_covered_by_header, on_change
    in
    let apply_action
          ~inject:_
          ~schedule_event
          ( collated
          , (range_start, range_end)
          , scroll_control
          , rows_covered_by_header
          , on_change )
          (model : Model.t)
          action
      =
      let scroll_if_some triple ~how =
        Option.iter triple ~f:(fun { Triple.id; index; _ } ->
          let id =
            if index < range_start + rows_covered_by_header
            then (
              match find_by_index collated ~index:(index - rows_covered_by_header) with
              | Some { id; _ } -> id
              | None -> id)
            else id
          in
          schedule_event (scroll_control (how id)));
        triple, None
      in
      let new_focus, new_shadow =
        match (action : Action.t) with
        | Select key ->
          scroll_if_some (find_by_key ~key ~key_equal:Key.equal collated) ~how:(fun id ->
            Scroll.To (id, `Minimal))
        | Unfocus ->
          (match model.current with
           (* already unfocused *)
           | None -> None, model.shadow
           | Some current -> None, Some current)
        | Down ->
          scroll_if_some
            (match model with
             | { current = None; shadow = Some { index; _ } } ->
               first_some
                 [ lazy (find_by_index collated ~index:(index + 1))
                 ; lazy (find_by_index collated ~index)
                 ]
             | { current = None; shadow = None } ->
               find_by_index collated ~index:range_start
             | { current = Some { Triple.key; _ }; _ } ->
               let%bind.Option { Triple.index; _ } =
                 find_by_key collated ~key ~key_equal:Key.equal
               in
               Option.first_some (find_by_index collated ~index:(index + 1)) model.current)
            ~how:(fun id -> To (id, `Minimal))
        | Up ->
          scroll_if_some
            (match model with
             | { current = None; shadow = Some { index; _ } } ->
               find_by_index collated ~index:(index - 1)
             | { current = None; shadow = None } ->
               let index =
                 Int.min
                   range_end
                   (Collated.num_after_range collated + Collated.length collated)
               in
               first_some
                 [ lazy (find_by_index collated ~index:(index - 1))
                 ; lazy (find_by_index collated ~index)
                 ]
             | { current = Some { Triple.key; _ }; _ } ->
               let%bind.Option { Triple.index; _ } =
                 find_by_key collated ~key ~key_equal:Key.equal
               in
               Option.first_some (find_by_index collated ~index:(index - 1)) model.current)
            ~how:(fun id -> To (id, `Minimal))
        | Page_down ->
          scroll_if_some (find_by_index collated ~index:range_end) ~how:(fun id ->
            To (id, `To_top))
        | Page_up ->
          scroll_if_some (find_by_index collated ~index:range_start) ~how:(fun id ->
            To (id, `To_bottom))
        | Recompute ->
          let new_focus =
            Option.bind model.current ~f:(fun { key; id; index } ->
              match
                find_in_range
                  ~range:(range_start, range_end)
                  ~collated
                  ~key
                  ~id
                  ~index
                  ~key_equal:Key.equal
              with
              | Yes -> model.current
              | No_but_this_one_is triple -> Some triple
              | Indeterminate -> None)
          in
          new_focus, model.shadow
      in
      let prev_key = model.current |> Option.map ~f:(fun t -> t.key)
      and next_key = new_focus |> Option.map ~f:(fun t -> t.key) in
      if not ([%equal: Key.t option] prev_key next_key)
      then schedule_event (on_change next_key);
      { Model.current = new_focus; shadow = new_shadow }
    in
    let%sub ((model, inject) as machine) =
      Bonsai.Incr.model_cutoff
      @@ Bonsai.state_machine1
           [%here]
           (module Model)
           (module Action)
           ~default_model:Model.empty
           ~apply_action
           input
    in
    let module Forces_recalculation = struct
      type t =
        { range : int * int
        ; num_before : int
        ; num_contained : int
        }
      [@@deriving equal, sexp]
    end
    in
    let%sub () =
      Bonsai.Edge.on_change
        [%here]
        (module Forces_recalculation)
        (let%map range = range
         and collated = collated in
         { Forces_recalculation.range
         ; num_before = Collated.num_before_range collated
         ; num_contained = Collated.num_filtered_rows collated
         })
        ~callback:
          (let%map inject = inject in
           fun _ -> inject Recompute)
    in
    let%sub () =
      (* The current focus being removed is a special case which requires unfocusing.
         This has two effects:
         - The model's "current"  will be set to None, which is what consumers
           of the API will expect.
         - Unfocus sets the "shadow" value, which is used to resume focus when the
           user hits "up" or "down". *)
      let%sub is_present =
        Bonsai.Incr.compute (Bonsai.Value.both model remapped) ~f:(fun both ->
          match%pattern_bind.Ui_incr both with
          | { current = Some { key = current; _ }; _ }, map ->
            let lookup = Ui_incr.Map.Lookup.create map ~comparator:Key.comparator in
            let%bind.Ui_incr current = current in
            Ui_incr.map (Ui_incr.Map.Lookup.find lookup current) ~f:Option.is_some
          | { current = None; _ }, _ -> Ui_incr.return false)
      in
      Bonsai.Edge.on_change'
        [%here]
        (module Bool)
        is_present
        ~callback:
          (let%map _, inject = machine in
           fun prev cur ->
             match prev, cur with
             (* The currently-focused value being removed causes an unfocus event. *)
             | Some true, false -> inject Unfocus
             | _ -> Effect.Ignore)
    in
    let%arr model, inject = machine in
    { By_row.focused = Option.map model.current ~f:(fun { key; _ } -> key)
    ; unfocus = inject Unfocus
    ; focus_up = inject Up
    ; focus_down = inject Down
    ; page_up = inject Page_up
    ; page_down = inject Page_down
    ; focus = (fun k -> inject (Select k))
    }
  ;;
end

let component
  : type kind key.
    (kind, key) Kind.t
    -> (key, _) Bonsai.comparator
    -> collated:(key, _) Collated.t Value.t
    -> rows_covered_by_header:int Value.t
    -> range:_
    -> remapped:(key, _, _) Map.t Value.t
    -> path:_
    -> kind Computation.t
  =
  fun kind ->
  match kind with
  | None ->
    fun _ ~collated:_ ~rows_covered_by_header:_ ~range:_ ~remapped:_ ~path:_ ->
      Bonsai.const ()
  | By_row { on_change } -> Row_machine.component ~on_change
;;

let get_focused (type r k) : (r, k) Kind.t -> r Value.t -> k option Value.t =
  fun kind value ->
  match kind with
  | None -> Bonsai.Value.return None
  | By_row _ ->
    let%map { focused; _ } = value in
    focused
;;

let get_on_row_click (type r k) (kind : (r, k) Kind.t) (value : r Value.t)
  : (k -> unit Effect.t) Value.t
  =
  match kind with
  | None -> Value.return (fun _ -> Effect.Ignore)
  | By_row _ ->
    let%map { focus; _ } = value in
    focus
;;

module For_testing = struct
  module Range_response = Row_machine.Range_response
  module Triple = Row_machine.Triple

  let find_in_range = Row_machine.find_in_range
end
