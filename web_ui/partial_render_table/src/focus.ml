open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Collated = Incr_map_collate.Collated

module By_cell = struct
  module Action = struct
    type ('key, 'column_id) t =
      | Unfocus
      | Up
      | Down
      | Left
      | Right
      | Page_up
      | Page_down
      | Select of ('key * 'column_id)
      | Select_index of (int * 'column_id)
      | Switch_from_index_to_key of
          { key : 'key
          ; index : int
          }
    [@@deriving sexp_of]
  end

  type ('k, 'column_id, 'presence) t =
    { focused : 'presence
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; focus_left : unit Effect.t
    ; focus_right : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> 'column_id -> unit Effect.t
    ; focus_index : int -> 'column_id -> unit Effect.t
    }
  [@@deriving fields ~getters]

  type ('k, 'column_id) optional = ('k, 'column_id, ('k * 'column_id) option) t

  module Without_presence : sig
    (* The effects computed from [inject] are constant, and only computed once.
       [presence], however, changes extremely frequently, since it is the focused row.
       Constructing this value in stages means that downstream consumers that only look at
       e.g. "focus_up", won't have cutoff issues caused by [inject Up] being called every
       time that the model changes. *)
    type ('k, 'column_id) unfinalized

    val create
      :  (('k, 'column_id) Action.t -> unit Effect.t)
      -> ('k, 'column_id) unfinalized

    val finalize
      :  ('k, 'column_id) unfinalized
      -> 'presence
      -> ('k, 'column_id, 'presence) t
  end = struct
    type ('k, 'column_id) unfinalized = ('k, 'column_id, Nothing.t option) t

    let create inject =
      { focused = None
      ; unfocus = inject Action.Unfocus
      ; focus_up = inject Up
      ; focus_down = inject Down
      ; focus_left = inject Left
      ; focus_right = inject Right
      ; page_up = inject Page_up
      ; page_down = inject Page_down
      ; focus = (fun k c -> inject (Select (k, c)))
      ; focus_index = (fun k c -> inject (Select_index (k, c)))
      }
    ;;

    let finalize without_focus focused = { without_focus with focused }
  end
end

module By_row = struct
  type ('k, 'presence) t = ('k, unit, 'presence) By_cell.t
  type 'k optional = ('k, 'k option) t

  let focused = By_cell.focused
  let unfocus = By_cell.unfocus
  let focus_up = By_cell.focus_up
  let focus_down = By_cell.focus_down
  let page_up = By_cell.page_up
  let page_down = By_cell.page_down
  let focus t k = By_cell.focus t k ()
  let focus_index t i = By_cell.focus_index t i ()

  module Expert = struct
    let keyless (t : ('k, 'presence) t) =
      { t with focused = None; focus = (fun _ _ -> Effect.Ignore) }
    ;;
  end
end

module Kind = struct
  type ('a, 'presence, 'k, 'column_id) t =
    | None : (unit, unit, 'k, 'column_id) t
    | By_row :
        { on_change : ('k option -> unit Effect.t) Value.t
        ; compute_presence : 'k option Value.t -> 'presence Computation.t
        }
        -> (('k, 'presence) By_row.t, 'presence, 'k, 'column_id) t
    | By_cell :
        { on_change : (('k * 'column_id) option -> unit Effect.t) Value.t
        ; compute_presence : ('k * 'column_id) option Value.t -> 'presence Computation.t
        }
        -> (('k, 'column_id, 'presence) By_cell.t, 'presence, 'k, 'column_id) t
end

type ('key, 'column_id, 'kind) focused =
  | Nothing_focused : ('key, 'column_id, _) focused
  | Cell_focused : ('key * 'column_id) -> ('key, 'column_id, _ By_cell.t) focused
  | Row_focused : 'key -> ('key, 'column_id, _ By_row.t) focused

type ('kind, 'key, 'column_id) t =
  { focus : 'kind
  ; visually_focused : ('key, 'column_id, 'kind) focused
  }

module Cell_machine = struct
  module Triple = struct
    (** This type is pretty integral to the row selection state-machine.  A
        value of this type is stored as the "currently selected row" and also
        as the result for "next row down" queries.  *)
    type 'k t =
      { key : 'k
      ; index : int
      }
    [@@deriving sexp, equal]
  end

  module Currently_selected_row = struct
    type 'k t =
      | At_key of 'k Triple.t
      | At_index of int
    [@@deriving sexp, equal]
  end

  module Action = By_cell.Action

  let find_by collated ~f =
    with_return_option (fun { return } ->
      let i = ref (Collated.num_before_range collated) in
      collated
      |> Collated.to_opaque_map
      |> Map.iter ~f:(fun (key, _) ->
           if f ~key ~index:!i then return { Triple.key; index = !i } else Int.incr i))
  ;;

  let find_by_key collated ~key:needle ~key_equal =
    find_by collated ~f:(fun ~key ~index:_ -> key_equal key needle)
  ;;

  let find_by_index collated ~index:needle =
    with_return (fun { return } ->
      find_by collated ~f:(fun ~key:_ ~index ->
        if index > needle then return None;
        Int.equal index needle))
  ;;

  module Currently_selected_cell = struct
    type ('k, 'column_id) t =
      { row : 'k Currently_selected_row.t
      ; column : 'column_id
      }
    [@@deriving sexp_of, equal]
  end

  let component
    (type key column_id data cmp column_id_cmp presence)
    (key : (key, cmp) Bonsai.comparator)
    (column_id : (column_id, column_id_cmp) Bonsai.comparator)
    ~(compute_presence : (key * column_id) option Value.t -> presence Computation.t)
    ~(on_change : ((key * column_id) option -> unit Effect.t) Value.t)
    ~(collated : (key, data) Incr_map_collate.Collated.t Value.t)
    ~(columns : column_id list Value.t)
    ~(range : (int * int) Value.t)
    ~(scroll_to_index : (int -> unit Effect.t) Value.t)
    ~(scroll_to_column : (column_id -> unit Effect.t) Value.t)
    : ((key, column_id, presence) By_cell.t, key, column_id) t Computation.t
    =
    let module Key = struct
      include (val key)

      let equal = Comparable.equal comparator.compare
    end
    in
    let module Column_id = struct
      include (val column_id)

      let equal = Comparable.equal comparator.compare
    end
    in
    let module Action = struct
      include Action

      type t = (Key.t, Column_id.t) Action.t [@@deriving sexp_of]
    end
    in
    let module Model = struct
      (** [current] is the currently selected cell.
          [shadow] is the previously selected cell.

          Shadow is useful for computing "next cell down" if the user previously
          unfocused, or if the element that was previously selected has been
          removed. *)
      type t =
        | No_focused_cell
        | Shadow of (Key.t, Column_id.t) Currently_selected_cell.t
        | Visible of (Key.t, Column_id.t) Currently_selected_cell.t
      [@@deriving sexp_of, equal]

      let empty = No_focused_cell
    end
    in
    let module Input = struct
      type t =
        { collated : (key, data) Collated.t
        ; columns : column_id list
        ; range : int * int
        ; on_change : (key * column_id) option -> unit Ui_effect.t
        ; scroll_to_index : int -> unit Effect.t
        ; scroll_to_column : column_id -> unit Effect.t
        }
    end
    in
    let%sub input =
      let%arr collated = collated
      and columns = columns
      and range = range
      and on_change = on_change
      and scroll_to_index = scroll_to_index
      and scroll_to_column = scroll_to_column in
      { Input.collated; columns; range; on_change; scroll_to_index; scroll_to_column }
    in
    let apply_action context input (model : Model.t) action =
      match input with
      | Bonsai.Computation_status.Active
          { Input.columns = []
          ; collated = _
          ; range = _
          ; on_change = _
          ; scroll_to_index = _
          ; scroll_to_column = _
          } ->
        (* There are no columns, therefore no cells, so there is nothing to focus. *)
        Model.No_focused_cell
      | Inactive ->
        (* We want to preserve focus state even if the table is inactive *)
        model
      | Active
          { collated
          ; columns = first_column :: _ as columns
          ; range = range_start, range_end
          ; on_change
          ; scroll_to_index
          ; scroll_to_column
          } ->
        let scroll_to_index index =
          Bonsai.Apply_action_context.schedule_event context (scroll_to_index index)
        in
        let scroll_to_column column =
          Bonsai.Apply_action_context.schedule_event context (scroll_to_column column)
        in
        let update_focus ~f =
          let original_index_and_column =
            match model with
            | No_focused_cell -> None
            | Shadow { row; column } | Visible { row; column } ->
              (match row with
               | At_index index -> Some (index, column)
               | At_key { key; index = old_index } ->
                 (match find_by_key collated ~key ~key_equal:Key.equal with
                  | Some { Triple.index; key = _ } -> Some (index, column)
                  | None -> Some (old_index, column)))
          in
          let new_index, new_column = f original_index_and_column in
          scroll_to_index new_index;
          scroll_to_column new_column;
          let new_index =
            Int.max
              0
              (Int.min
                 new_index
                 (Collated.num_before_range collated
                  + Collated.num_after_range collated
                  + Collated.length collated
                  - 1))
          in
          let row =
            match find_by_index collated ~index:new_index with
            | Some triple -> Currently_selected_row.At_key triple
            | None -> At_index new_index
          in
          { Currently_selected_cell.row; column = new_column }
        in
        let new_focus =
          match (action : Action.t) with
          | Switch_from_index_to_key { key; index } ->
            (* Before switching from index to key, we need to make sure that
               the focus is still at the that index. If it isn't, then we
               ignore the request to switch from index to key, since it is out
               of date. *)
            (match model with
             | No_focused_cell -> None
             | Visible { row = At_index model_index; column }
             | Shadow { row = At_index model_index; column } ->
               if model_index = index
               then Some { Currently_selected_cell.row = At_key { key; index }; column }
               else Some { row = At_index model_index; column }
             | Visible ({ row = At_key _; column = _ } as current)
             | Shadow ({ row = At_key _; column = _ } as current) -> Some current)
          | Select (key, column) ->
            (match find_by_key ~key ~key_equal:Key.equal collated with
             | Some ({ index; key = _ } as triple) ->
               scroll_to_index index;
               Some { row = At_key triple; column }
             | None -> None)
          | Unfocus -> None
          | Select_index (new_index, new_column) ->
            Some (update_focus ~f:(fun _original_index -> new_index, new_column))
          | Down ->
            Some
              (update_focus ~f:(function
                | Some (original_index, column) -> original_index + 1, column
                | None -> range_start, first_column))
          | Up ->
            Some
              (update_focus ~f:(function
                | Some (original_index, column) -> original_index - 1, column
                | None -> range_end, first_column))
          | Left ->
            Some
              (update_focus ~f:(function
                | Some (original_index, original_column) ->
                  let column_index =
                    List.findi columns ~f:(fun _ column ->
                      Column_id.equal column original_column)
                  in
                  (match column_index with
                   | Some (i, _) ->
                     (* List.nth_exn will throw if and only if the length of the list is 0,
                         which the pattern match above guards against already.*)
                     original_index, List.nth_exn columns (Int.max (i - 1) 0)
                   | None -> original_index, first_column)
                | None -> range_start, first_column))
          | Right ->
            Some
              (update_focus ~f:(function
                | Some (original_index, original_column) ->
                  let column_index =
                    List.findi columns ~f:(fun _ column ->
                      Column_id.equal column original_column)
                  in
                  (match column_index with
                   | Some (i, _) ->
                     (* List.nth_exn will throw if and only if the length of the list is 0,
                         which the pattern match above guards against already.*)
                     ( original_index
                     , List.nth_exn columns (Int.min (i + 1) (List.length columns - 1)) )
                   | None -> original_index, first_column)
                | None -> range_start, first_column))
          | Page_down ->
            Some
              (update_focus ~f:(function
                | Some (original_index, original_column) ->
                  let new_index =
                    if original_index < range_end
                    then range_end
                    else original_index + (range_end - range_start)
                  in
                  new_index, original_column
                | None -> range_end, first_column))
          | Page_up ->
            Some
              (update_focus ~f:(function
                | Some (original_index, original_column) ->
                  let new_index =
                    if original_index > range_start
                    then range_start
                    else original_index - (range_end - range_start)
                  in
                  new_index, original_column
                | None -> range_start, first_column))
        in
        let new_model =
          match action with
          | Unfocus ->
            (match model with
             | No_focused_cell -> Model.No_focused_cell
             | Visible triple | Shadow triple -> Shadow triple)
          | _ ->
            (match new_focus with
             | Some triple -> Visible triple
             | None -> No_focused_cell)
        in
        let prev_key =
          match model with
          | No_focused_cell | Shadow _ | Visible { row = At_index _; column = _ } -> None
          | Visible { row = At_key { key; _ }; column } -> Some (key, column)
        in
        let next_key =
          match new_model with
          | No_focused_cell | Shadow _ | Visible { row = At_index _; column = _ } -> None
          | Visible { row = At_key { key; _ }; column } -> Some (key, column)
        in
        if not ([%equal: (Key.t * Column_id.t) option] prev_key next_key)
        then Bonsai.Apply_action_context.schedule_event context (on_change next_key);
        new_model
    in
    let%sub current, inject =
      Bonsai.state_machine1
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
        ~sexp_of_action:[%sexp_of: Action.t]
        ~default_model:Model.empty
        ~apply_action
        input
    in
    let%sub current = return (Value.cutoff ~equal:[%equal: Model.t] current) in
    let%sub everything_injectable =
      (* By depending on only [inject] (which is a constant), we can build the vast majority
         of this record, leaving only the "focused" field left unset, which we quickly fix.
         Doing it this way will mean that downstream consumers that only look at e.g. the "focus_up"
         field, won't have cutoff issues caused by [inject Up] being called every time that
         the model changes. *)
      let%arr inject = inject in
      By_cell.Without_presence.create inject
    in
    let%sub visually_focused =
      let%arr current = current
      and collated = collated in
      match current with
      | Visible { row = At_key { key; _ }; column } -> Some (key, column)
      | Visible { row = At_index index; column } ->
        (match find_by_index collated ~index with
         | Some { key; _ } -> Some (key, column)
         | None -> None)
      | No_focused_cell | Shadow _ -> None
    in
    let%sub () =
      Bonsai.Edge.on_change
        ~sexp_of_model:[%sexp_of: Model.t * (Key.t * Column_id.t) option]
        ~equal:[%equal: Model.t * (Key.t * Column_id.t) option]
        (Value.both current visually_focused)
        ~callback:
          (let%map inject = inject in
           fun (current, visually_focused) ->
             (* If we ever notice that the state machine is focused at an index
                for which there is an existing row, we can request that the state
                machine switch over to being focused on the key at that index. *)
             match current with
             | Model.Visible { row = At_index index; column = _ } ->
               (match visually_focused with
                | Some (key, _) -> inject (Switch_from_index_to_key { key; index })
                | None -> Effect.Ignore)
             | Visible { row = At_key _; column = _ } | No_focused_cell | Shadow _ ->
               Effect.Ignore)
    in
    let%sub presence = compute_presence visually_focused in
    let%sub visually_focused =
      match%arr visually_focused with
      | None -> Nothing_focused
      | Some (key, column_id) -> Cell_focused (key, column_id)
    in
    let%arr presence = presence
    and visually_focused = visually_focused
    and everything_injectable = everything_injectable in
    let focus = By_cell.Without_presence.finalize everything_injectable presence in
    { focus; visually_focused }
  ;;
end

module Row_machine = struct
  let component
    (type key data cmp presence)
    (key : (key, cmp) Bonsai.comparator)
    ~(compute_presence : key option Value.t -> presence Computation.t)
    ~(on_change : (key option -> unit Effect.t) Value.t)
    ~(collated : (key, data) Incr_map_collate.Collated.t Value.t)
    ~(range : (int * int) Value.t)
    ~(scroll_to_index : (int -> unit Effect.t) Value.t)
    : ((key, presence) By_row.t, key, _) t Computation.t
    =
    let compute_presence focus =
      let%sub focus =
        let%arr focus = focus in
        Option.map focus ~f:(fun (key, ()) -> key)
      in
      compute_presence focus
    in
    let%sub on_change =
      let%arr on_change = on_change in
      fun focus ->
        match focus with
        | None -> on_change None
        | Some (key, ()) -> on_change (Some key)
    in
    let%sub { focus; visually_focused } =
      Cell_machine.component
        key
        (module Unit)
        ~on_change
        ~compute_presence
        ~collated
        ~columns:(Value.return [ () ])
        ~range
        ~scroll_to_index
        ~scroll_to_column:(Value.return (fun _ -> Effect.Ignore))
    in
    let%sub visually_focused =
      let%arr visually_focused = visually_focused in
      match visually_focused with
      | Nothing_focused -> Nothing_focused
      | Cell_focused (key, ()) | Row_focused key -> Row_focused key
    in
    let%arr focus = focus
    and visually_focused = visually_focused in
    { focus; visually_focused }
  ;;
end

let component
  : type kind presence key column_id column_id_cmp.
    (kind, presence, key, column_id) Kind.t
    -> (key, _) Bonsai.comparator
    -> (column_id, column_id_cmp) Bonsai.comparator
    -> collated:(key, _) Collated.t Value.t
    -> leaves:column_id Header_tree.leaf list Value.t
    -> range:_
    -> scroll_to_index:_
    -> scroll_to_column:(column_id -> unit Effect.t) Value.t
    -> (kind, key, column_id) t Computation.t
  =
  fun kind ->
  match kind with
  | None ->
    fun _ _ ~collated:_ ~leaves:_ ~range:_ ~scroll_to_index:_ ~scroll_to_column:_ ->
      Bonsai.const { focus = (); visually_focused = Nothing_focused }
  | By_row { on_change; compute_presence } ->
    fun key _ ~collated ~leaves:_ ~range ~scroll_to_index ~scroll_to_column:_ ->
      Row_machine.component
        key
        ~on_change
        ~compute_presence
        ~collated
        ~range
        ~scroll_to_index
  | By_cell { on_change; compute_presence } ->
    fun key column ~collated ~leaves ~range ~scroll_to_index ~scroll_to_column ->
      let%sub columns =
        let%arr leaves = leaves in
        List.map leaves ~f:(fun leaf -> leaf.column_id)
      in
      Cell_machine.component
        key
        column
        ~on_change
        ~compute_presence
        ~collated
        ~columns
        ~range
        ~scroll_to_index
        ~scroll_to_column
;;

let get_on_cell_click
  (type r presence k column_id)
  (kind : (r, presence, k, column_id) Kind.t)
  (value : r Value.t)
  : (k -> column_id -> unit Effect.t) Value.t
  =
  (* The incrementality of this code is important: we need to have a cutoff between when
     we extract the focus function and when we create the wrapping function. This is
     because the focus function should be a constant, but the entire [control] is not. *)
  match kind with
  | None -> Value.return (fun _ _ -> Effect.Ignore)
  | By_row _ ->
    let focus =
      let%map control = value in
      By_row.focus control
    in
    let%map focus = focus in
    fun key _ -> focus key
  | By_cell _ ->
    let focus =
      let%map control = value in
      By_cell.focus control
    in
    let%map focus = focus in
    fun key column -> focus key column
;;
