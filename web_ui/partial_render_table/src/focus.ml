open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Collated = Incr_map_collate.Collated

module By_row = struct
  type ('k, 'presence) t =
    { focused : 'presence
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> unit Effect.t
    ; focus_index : int -> unit Effect.t
    }
  [@@deriving fields]

  type 'k optional = ('k, 'k option) t
end

module Kind = struct
  type ('a, 'presence, 'k) t =
    | None : (unit, unit, 'k) t
    | By_row :
        { on_change : ('k option -> unit Effect.t) Value.t
        ; compute_presence : 'k option Value.t -> 'presence Computation.t
        }
        -> (('k, 'presence) By_row.t, 'presence, 'k) t
end

type ('kind, 'key) t =
  { focus : 'kind
  ; visually_focused : 'key option
  }

module Row_machine = struct
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

  let find_by collated ~f =
    with_return_option (fun { return } ->
      let i = ref (Collated.num_before_range collated) in
      collated
      |> Collated.to_map_list
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

  module Action = struct
    type 'key t =
      | Unfocus
      | Up
      | Down
      | Page_up
      | Page_down
      | Select of 'key
      | Select_index of int
      | Switch_from_index_to_key of
          { key : 'key
          ; index : int
          }
    [@@deriving sexp_of]
  end

  let component
        (type key data cmp presence)
        (key : (key, cmp) Bonsai.comparator)
        ~(compute_presence : key option Value.t -> presence Computation.t)
        ~(on_change : (key option -> unit Effect.t) Value.t)
        ~(collated : (key, data) Incr_map_collate.Collated.t Value.t)
        ~(range : (int * int) Value.t)
        ~(scroll_to_index : (int -> unit Effect.t) Value.t)
    : ((key, presence) By_row.t, key) t Computation.t
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
        | No_focused_row
        | Shadow of Key.t Currently_selected_row.t
        | Visible of Key.t Currently_selected_row.t
      [@@deriving sexp, equal]

      let empty = No_focused_row
    end
    in
    let module Input = struct
      type t =
        { collated : (key, data) Collated.t
        ; range : int * int
        ; on_change : key option -> unit Ui_effect.t
        ; scroll_to_index : int -> unit Effect.t
        }
    end
    in
    let%sub input =
      let%arr collated = collated
      and range = range
      and on_change = on_change
      and scroll_to_index = scroll_to_index in
      { Input.collated; range; on_change; scroll_to_index }
    in
    let apply_action ~inject:_ ~schedule_event input (model : Model.t) action =
      match input with
      | Bonsai.Computation_status.Active
          { Input.collated; range = range_start, range_end; on_change; scroll_to_index }
        ->
        let scroll_to_index index = schedule_event (scroll_to_index index) in
        let update_focused_index ~f =
          let original_index =
            match model with
            | No_focused_row -> None
            | Shadow current | Visible current ->
              (match current with
               | At_index index -> Some index
               | At_key { key; index = old_index } ->
                 (match find_by_key collated ~key ~key_equal:Key.equal with
                  | Some { Triple.index; key = _ } -> Some index
                  | None -> Some old_index))
          in
          let new_index = f original_index in
          scroll_to_index new_index;
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
          Some
            (match find_by_index collated ~index:new_index with
             | Some triple -> Currently_selected_row.At_key triple
             | None -> At_index new_index)
        in
        let new_focus =
          match (action : Action.t) with
          | Switch_from_index_to_key { key; index } ->
            (* Before switching from index to key, we need to make sure that
               the focus is still at the that index. If it isn't, then we
               ignore the request to switch from index to key, since it is out
               of date. *)
            (match model with
             | No_focused_row -> None
             | Visible (At_index model_index) | Shadow (At_index model_index) ->
               if model_index = index
               then Some (Currently_selected_row.At_key { key; index })
               else Some (At_index model_index)
             | Visible (At_key _ as current) | Shadow (At_key _ as current) ->
               Some current)
          | Select key ->
            (match find_by_key ~key ~key_equal:Key.equal collated with
             | Some ({ index; key = _ } as triple) ->
               scroll_to_index index;
               Some (Currently_selected_row.At_key triple)
             | None -> None)
          | Unfocus -> None
          | Select_index new_index ->
            update_focused_index ~f:(fun _original_index -> new_index)
          | Down ->
            update_focused_index ~f:(function
              | Some original_index -> original_index + 1
              | None -> range_start)
          | Up ->
            update_focused_index ~f:(function
              | Some original_index -> original_index - 1
              | None -> range_end)
          | Page_down ->
            update_focused_index ~f:(function
              | Some original_index ->
                if original_index < range_end
                then range_end
                else original_index + (range_end - range_start)
              | None -> range_end)
          | Page_up ->
            update_focused_index ~f:(function
              | Some original_index ->
                if original_index > range_start
                then range_start
                else original_index - (range_end - range_start)
              | None -> range_start)
        in
        let new_model =
          match action with
          | Unfocus ->
            (match model with
             | No_focused_row -> Model.No_focused_row
             | Visible triple | Shadow triple -> Shadow triple)
          | _ ->
            (match new_focus with
             | Some triple -> Visible triple
             | None -> No_focused_row)
        in
        let prev_key =
          match model with
          | No_focused_row | Shadow _ | Visible (At_index _) -> None
          | Visible (At_key { key; _ }) -> Some key
        in
        let next_key =
          match new_model with
          | No_focused_row | Shadow _ | Visible (At_index _) -> None
          | Visible (At_key { key; _ }) -> Some key
        in
        if not ([%equal: Key.t option] prev_key next_key)
        then schedule_event (on_change next_key);
        new_model
      | Inactive ->
        eprint_s
          [%message
            [%here]
              "An action sent to a [state_machine1] has been dropped because its input \
               was not present. This happens when the [state_machine1] is inactive when \
               it receives a message."
              (action : Action.t)];
        model
    in
    let%sub current, inject =
      Bonsai.Incr.model_cutoff
      @@ Bonsai.state_machine1
           (module Model)
           (module Action)
           ~default_model:Model.empty
           ~apply_action
           input
    in
    let%sub everything_injectable =
      (* By depending on only [inject] (which is a constant), we can build the vast majority
         of this record, leaving only the "focused" field left unset, which we quickly fix.
         Doing it this way will mean that downstream consumers that only look at e.g. the "focus_up"
         field, won't have cutoff issues caused by [inject Up] being called every time that
         the model changes. *)
      let%arr inject = inject in
      { By_row.focused = None
      ; unfocus = inject Unfocus
      ; focus_up = inject Up
      ; focus_down = inject Down
      ; page_up = inject Page_up
      ; page_down = inject Page_down
      ; focus = (fun k -> inject (Select k))
      ; focus_index = (fun k -> inject (Select_index k))
      }
    in
    let%sub visually_focused =
      let%arr current = current
      and collated = collated in
      match current with
      | Visible (At_key { key; _ }) -> Some key
      | Visible (At_index index) ->
        (match find_by_index collated ~index with
         | Some { key; _ } -> Some key
         | None -> None)
      | No_focused_row | Shadow _ -> None
    in
    let%sub () =
      Bonsai.Edge.on_change
        (module struct
          type t = Model.t * Key.t option [@@deriving sexp, equal]
        end)
        (Value.both current visually_focused)
        ~callback:
          (let%map inject = inject in
           fun (current, visually_focused) ->
             (* If we ever notice that the state machine is focused at an index
                for which there is an existing row, we can request that the state
                machine switch over to being focused on the key at that index. *)
             match current with
             | Model.Visible (At_index index) ->
               (match visually_focused with
                | Some key -> inject (Switch_from_index_to_key { key; index })
                | None -> Effect.Ignore)
             | Visible (At_key _) | No_focused_row | Shadow _ -> Effect.Ignore)
    in
    let%sub presence = compute_presence visually_focused in
    let%arr presence = presence
    and visually_focused = visually_focused
    and everything_injectable = everything_injectable in
    let focus = { everything_injectable with By_row.focused = presence } in
    { focus; visually_focused }
  ;;
end

let component
  : type kind presence key.
    (kind, presence, key) Kind.t
    -> (key, _) Bonsai.comparator
    -> collated:(key, _) Collated.t Value.t
    -> range:_
    -> scroll_to_index:_
    -> (kind, key) t Computation.t
  =
  fun kind ->
  match kind with
  | None ->
    fun _ ~collated:_ ~range:_ ~scroll_to_index:_ ->
      Bonsai.const { focus = (); visually_focused = None }
  | By_row { on_change; compute_presence } ->
    Row_machine.component ~on_change ~compute_presence
;;

let get_focused (type r presence k)
  : (r, presence, k) Kind.t -> r Value.t -> presence Value.t
  =
  fun kind value ->
  match kind with
  | None -> Value.return ()
  | By_row _ ->
    let%map { focused; _ } = value in
    focused
;;

let get_on_row_click
      (type r presence k)
      (kind : (r, presence, k) Kind.t)
      (value : r Value.t)
  : (k -> unit Effect.t) Value.t
  =
  match kind with
  | None -> Value.return (fun _ -> Effect.Ignore)
  | By_row _ ->
    let%map { focus; _ } = value in
    focus
;;
