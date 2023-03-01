open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Scroll = Bonsai_web_ui_scroll_utilities
module Collated = Incr_map_collate.Collated
module Map_list = Incr_map_collate.Map_list

module By_row = struct
  type ('k, 'presence) t =
    { focused : 'presence
    ; unfocus : unit Effect.t
    ; focus_up : unit Effect.t
    ; focus_down : unit Effect.t
    ; page_up : unit Effect.t
    ; page_down : unit Effect.t
    ; focus : 'k -> unit Effect.t
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
      ; id : Map_list.Key.t
      ; index : int
      }
    [@@deriving sexp, equal]
  end

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

  module Action = struct
    type 'key t =
      | Unfocus
      | Up
      | Down
      | Page_up
      | Page_down
      | Select of 'key
    [@@deriving sexp_of]
  end

  let component
        (type key data cmp presence)
        (key : (key, cmp) Bonsai.comparator)
        ~(compute_presence : key option Value.t -> presence Computation.t)
        ~(on_change : (key option -> unit Effect.t) Value.t)
        ~(collated : (key, data) Incr_map_collate.Collated.t Value.t)
        ~(header_height : int Value.t)
        ~(row_height : int)
        ~(range : (int * int) Value.t)
        ~(midpoint_of_container : int Value.t)
        ~path
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
        | Shadow of Key.t Triple.t
        | Visible of Key.t Triple.t
      [@@deriving sexp, equal]

      let empty = No_focused_row
    end
    in
    let module Input = struct
      type t =
        { collated : (key, data) Collated.t
        ; range : int * int
        ; path : string
        ; header_height : int
        ; on_change : key option -> unit Ui_effect.t
        ; midpoint_of_container : int
        }
    end
    in
    let%sub input =
      let%arr collated = collated
      and path = path
      and range = range
      and header_height = header_height
      and on_change = on_change
      and midpoint_of_container = midpoint_of_container in
      { Input.collated; range; path; header_height; on_change; midpoint_of_container }
    in
    let scroll_if_some
          ~header_height
          ~range_start
          ~range_end
          ~schedule_event
          ~selector
          ~force_position
          ~midpoint_of_container
          triple
      =
      (* [scroll_if_some] scrolls to the row described by [triple]. *)
      Option.bind triple ~f:(fun { Triple.id = _; index; _ } ->
        let to_top =
          (* scrolling this row to the top of the display involves
             scrolling to a pixel that is actually [header_height] _above_
             the target row. *)
          Some ((row_height * index) - header_height)
        in
        let to_bottom =
          (* scroll to the bottom of this row means scrolling to the top of
             a one-pixel element just below this row *)
          Some (row_height * (index + 1))
        in
        match force_position with
        | Some `To_top -> to_top
        | None when index <= range_start -> to_top
        | Some `To_bottom -> to_bottom
        | None when index >= range_end -> to_bottom
        | _ -> None)
      |> Option.iter ~f:(fun y_px ->
        Scroll.to_position_inside_element
          ~x_px:midpoint_of_container
          ~y_px
          ~selector
          `Minimal
        |> Effect.ignore_m
        |> schedule_event);
      triple
    in
    let apply_action ~inject:_ ~schedule_event input (model : Model.t) action =
      match input with
      | Bonsai.Computation_status.Active
          { Input.collated
          ; range = range_start, range_end
          ; path
          ; header_height
          ; on_change
          ; midpoint_of_container
          } ->
        let selector = ".partial-render-table-" ^ path ^ " > div" in
        let scroll_if_some =
          scroll_if_some
            ~header_height
            ~range_start
            ~range_end
            ~schedule_event
            ~selector
            ~midpoint_of_container
        in
        let new_focus =
          match (action : Action.t) with
          | Select key ->
            scroll_if_some
              ~force_position:None
              (find_by_key ~key ~key_equal:Key.equal collated)
          | Unfocus -> None
          | Down ->
            let next =
              match model with
              | Shadow { index; _ } ->
                (* If we don't have a current focus, but there is a shadow-focus, try to
                   focus the element below it, and if that's not available, then attempting
                   to focus the shadow row is fine too. *)
                first_some
                  [ lazy (find_by_index collated ~index:(index + 1))
                  ; lazy (find_by_index collated ~index)
                  ]
              | No_focused_row ->
                (* "down" when completely unfocused starts at the top of the visible screen *)
                find_by_index collated ~index:range_start
              | Visible ({ Triple.key; index = old_index; _ } as current) ->
                (* start by finding the index of the row we're currently focused on *)
                (match find_by_key collated ~key ~key_equal:Key.equal with
                 | Some { Triple.index; _ } ->
                   (* if we find it... *)
                   Option.first_some
                     (* try to get the next one *)
                     (find_by_index collated ~index:(index + 1))
                     (* if we can't, stay where we are *)
                     (Some current)
                 | None ->
                   (* if it's gone, refocus at the previous index. *)
                   find_by_index collated ~index:old_index)
            in
            scroll_if_some next ~force_position:None
          | Up ->
            let next =
              match model with
              | Shadow { index; _ } ->
                first_some
                  [ lazy (find_by_index collated ~index:(index - 1))
                  ; lazy (find_by_index collated ~index)
                  ]
              | No_focused_row ->
                let index =
                  Int.min
                    range_end
                    (* range_end can be bigger than the actual length of the table, so
                       clamp it to the max size of the table *)
                    (Collated.num_after_range collated + Collated.length collated)
                in
                first_some
                  [ lazy (find_by_index collated ~index:(index - 1))
                  ; lazy (find_by_index collated ~index)
                  ]
              | Visible ({ Triple.key; index = old_index; _ } as current) ->
                (match find_by_key collated ~key ~key_equal:Key.equal with
                 | Some { Triple.index; _ } ->
                   Option.first_some
                     (find_by_index collated ~index:(index - 1))
                     (Some current)
                 | None -> find_by_index collated ~index:(old_index - 1))
            in
            scroll_if_some ~force_position:None next
          | Page_down ->
            scroll_if_some
              ~force_position:(Some `To_top)
              (find_by_index collated ~index:range_end)
          | Page_up ->
            scroll_if_some
              ~force_position:(Some `To_bottom)
              (find_by_index collated ~index:range_start)
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
          | No_focused_row | Shadow _ -> None
          | Visible { key; _ } -> Some key
        in
        let next_key =
          match new_model with
          | No_focused_row | Shadow _ -> None
          | Visible { key; _ } -> Some key
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
      }
    in
    let%sub current =
      match%arr current with
      | Visible { key; _ } -> Some key
      | No_focused_row | Shadow _ -> None
    in
    let%sub presence = compute_presence current in
    let%arr presence = presence
    and current = current
    and everything_injectable = everything_injectable in
    let focus = { everything_injectable with By_row.focused = presence } in
    { focus; visually_focused = current }
  ;;
end

let component
  : type kind presence key.
    (kind, presence, key) Kind.t
    -> (key, _) Bonsai.comparator
    -> collated:(key, _) Collated.t Value.t
    -> header_height:int Value.t
    -> row_height:int
    -> range:_
    -> midpoint_of_container:_
    -> path:_
    -> (kind, key) t Computation.t
  =
  fun kind ->
  match kind with
  | None ->
    fun _
      ~collated:_
      ~header_height:_
      ~row_height:_
      ~range:_
      ~midpoint_of_container:_
      ~path:_ ->
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
