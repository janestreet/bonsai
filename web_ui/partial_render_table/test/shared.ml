open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Indexed_column_id = Bonsai_web_ui_partial_render_table.Indexed_column_id
module Table = Bonsai_web_ui_partial_render_table.Basic
module Table_expert = Bonsai_web_ui_partial_render_table.Expert
module Sort_state = Bonsai_web_ui_partial_render_table_protocol.Sort_state

module Action = struct
  type 'column_id t =
    | Unfocus
    | Lock_focus
    | Unlock_focus
    | Focus_down
    | Focus_up
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus_row of int
    | Focus_cell of int * int
    | Focus_index of int
    | Set_column_width of
        { column_id : 'column_id
        ; width : float
        }
end

type t =
  { a : string
  ; b : float
  ; c : string
  ; d : int option
  ; e : string
  }

(* This is a "natural option sorting" comparison function that
   always sorts Nones to the bottom *)
let special_compare_option how compare_inner a b =
  match a, b with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some a, Some b ->
    (match how with
     | `Ascending -> compare_inner a b
     | `Descending -> -compare_inner a b)
;;

let columns ?(use_legacy_header = false) ~is_column_b_visible () =
  let module Columns = Table.Columns.Dynamic_cells in
  let render_header str =
    if use_legacy_header
    then Value.return (Columns.Sortable.Header.Legacy.wrap_with_icon (Vdom.Node.text str))
    else Value.return (Columns.Sortable.Header.with_icon (Vdom.Node.text str))
  in
  [ Columns.column
      ~header:(render_header "key")
      ~sort:
        (Value.return (fun a b ->
           Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b))
      ~cell:(fun ~key ~data:_ ->
        let%arr key = key in
        Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~header:(render_header "a")
      ~cell:(fun ~key:_ ~data ->
        let%sub state =
          Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
        in
        let%arr { a; _ } = data
        and state, set_state = state in
        Vdom.Node.div
          [ Vdom.Node.input ~attrs:[ Vdom.Attr.on_input (fun _ -> set_state) ] ()
          ; Vdom.Node.textf "%s %s" a state
          ])
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~header:(render_header "b")
      ~sort:
        (Value.return (fun a b_1 ->
           Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b) a b_1))
      ~cell:(fun ~key:_ ~data ->
        let%arr { b; _ } = data in
        Vdom.Node.textf "%f" b)
      ()
  ; Columns.column
      ~header:(render_header "d")
      ~sort:
        (Value.return (fun a b_1 ->
           Comparable.lift
             (special_compare_option `Ascending [%compare: int])
             ~f:(fun (_, { d; _ }) -> d)
             a
             b_1))
      ~sort_reversed:
        (Value.return (fun a b_1 ->
           Comparable.lift
             (special_compare_option `Descending [%compare: int])
             ~f:(fun (_, { d; _ }) -> d)
             a
             b_1))
      ~cell:(fun ~key:_ ~data ->
        let%arr { d; _ } = data in
        match d with
        | None -> Vdom.Node.textf "---"
        | Some d -> Vdom.Node.textf "%d" d)
      ()
  ]
;;

let columns_dynamic ~is_column_b_visible =
  let module Columns = Table.Columns.Dynamic_columns in
  let render_header text = Columns.Sortable.Header.with_icon (Vdom.Node.text text) in
  [ Columns.column
      ~header:(render_header "key")
      ~sort:(fun a b -> Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b)
      ~cell:(fun ~key ~data:_ -> Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~header:(render_header "a")
      ~sort:(fun a_1 b ->
        Comparable.lift [%compare: string] ~f:(fun (_, { a; _ }) -> a) a_1 b)
      ~cell:(fun ~key:_ ~data:{ a; _ } -> Vdom.Node.textf "%s" a)
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~header:(render_header "b")
      ~sort:(fun a b_1 ->
        Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b) a b_1)
      ~cell:(fun ~key:_ ~data:{ b; _ } -> Vdom.Node.textf "%f" b)
      ()
  ]
;;

let columns_dynamic_with_groups ~is_column_b_visible =
  let module Columns = Table.Columns.Dynamic_columns in
  let render_header str = Columns.Sortable.Header.with_icon (Vdom.Node.text str) in
  let cols =
    [ Columns.column
        ~header:(render_header "key")
        ~sort:(fun a b -> Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b)
        ~cell:(fun ~key ~data:_ -> Vdom.Node.textf "%d" key)
        ()
    ]
  in
  let group_1 =
    Columns.group
      ~label:(Vdom.Node.text "Basics")
      [ Columns.column
          ~header:(render_header "a")
          ~sort:(fun a_1 b ->
            Comparable.lift [%compare: string] ~f:(fun (_, { a; _ }) -> a) a_1 b)
          ~cell:(fun ~key:_ ~data:{ a; _ } -> Vdom.Node.textf "%s" a)
          ()
      ; Columns.column
          ~visible:is_column_b_visible
          ~header:(render_header "b")
          ~sort:(fun a b_1 ->
            Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b) a b_1)
          ~cell:(fun ~key:_ ~data:{ b; _ } -> Vdom.Node.textf "%f" b)
          ()
      ]
  in
  let level_2 =
    Columns.group
      ~label:(Vdom.Node.text "Level 2")
      [ Columns.column
          ~header:(render_header "c")
          ~sort:(fun a_1 b ->
            Comparable.lift [%compare: string] ~f:(fun (_, { c; _ }) -> c) a_1 b)
          ~cell:(fun ~key:_ ~data:{ c; _ } -> Vdom.Node.textf "%s" c)
          ()
      ; Columns.column
          ~visible:is_column_b_visible
          ~header:(render_header "d")
          ~sort:(fun a b_1 ->
            Comparable.lift [%compare: int option] ~f:(fun (_, { d; _ }) -> d) a b_1)
          ~cell:(fun ~key:_ ~data:{ d; _ } ->
            match d with
            | None -> Vdom.Node.textf "---"
            | Some d -> Vdom.Node.textf "%d" d)
          ()
      ]
  in
  let group_2_nested =
    Columns.group
      ~label:(Vdom.Node.text "Level 1")
      [ level_2
      ; Columns.column
          ~header:(render_header "e")
          ~sort:(fun a_1 b ->
            Comparable.lift [%compare: string] ~f:(fun (_, { e; _ }) -> e) a_1 b)
          ~cell:(fun ~key:_ ~data:{ e; _ } -> Vdom.Node.textf "%s" e)
          ()
      ]
  in
  cols @ [ group_1; group_2_nested ]
;;

let small_map =
  Int.Map.of_alist_exn
    [ 0, { a = "hello"; b = 1.0; c = "c"; d = Some 1; e = "x" }
    ; 1, { a = "there"; b = 2.0; c = "c"; d = Some 2; e = "y" }
    ; 4, { a = "world"; b = 2.0; c = "c"; d = None; e = "z" }
    ]
;;

let big_map =
  Int.Map.of_alist_exn
    (List.range 1 100
     |> List.map ~f:(fun i ->
          i, { a = "hi"; b = Float.of_int (i / 2); c = "apple"; d = Some 100; e = "1st" })
    )
;;

let groups_map =
  Int.Map.of_alist_exn
    [ 0, { a = "hello"; b = 1.0; c = "apple"; d = Some 100; e = "1st" }
    ; 1, { a = "there"; b = 2.0; c = "banana"; d = Some 200; e = "3rd" }
    ; 4, { a = "world"; b = 2.0; c = "pear"; d = None; e = "2nd" }
    ]
;;

module Test = struct
  type outer = t

  type ('a, 'column_id) t =
    { handle : ('a, 'column_id Action.t) Bonsai_web_test.Handle.t
    ; get_vdom : 'a -> Vdom.Node.t
    ; get_num_filtered_rows : 'a -> int option
    ; input_var : outer Int.Map.t Bonsai.Var.t
    ; filter_var : (key:int -> data:outer -> bool) Bonsai.Var.t
    }

  let focus_changed =
    Value.return (fun focus_changed_to ->
      Effect.print_s [%message (focus_changed_to : int option)])
  ;;

  let focus_changed' =
    Value.return (fun focus_changed_to ->
      Effect.print_s [%message (focus_changed_to : (int * Indexed_column_id.t) option)])
  ;;

  module Component = struct
    type ('a, 'focus, 'column_id) t =
      { component : 'a Computation.t
      ; get_vdom : 'a -> Vdom.Node.t
      ; get_inject : 'a -> 'column_id Action.t -> unit Ui_effect.t
      ; get_testing : 'a -> Bonsai_web_ui_partial_render_table.For_testing.t Lazy.t
      ; get_focus : 'a -> 'focus
      ; summarize_focus : ?num_filtered_rows:int -> 'focus -> string
      ; get_num_filtered_rows : 'a -> int option
      }

    let get_inject' t ~get_focus ~get_set_column_width =
      let focus = get_focus t in
      let module Focus_control = Table.Focus.By_row in
      function
      | Action.Unfocus -> Focus_control.unfocus focus
      | Lock_focus -> Focus_control.lock_focus focus
      | Unlock_focus -> Focus_control.unlock_focus focus
      | Focus_down -> Focus_control.focus_down focus
      | Focus_up -> Focus_control.focus_up focus
      | Page_up -> Focus_control.page_up focus
      | Page_down -> Focus_control.page_down focus
      | Focus_row k -> (Focus_control.focus focus) k
      | Focus_index index -> (Focus_control.focus_index focus) index
      | Focus_cell _ | Focus_left | Focus_right -> Effect.print_s [%message "Unsupported"]
      | Set_column_width { column_id; width } ->
        (get_set_column_width t) ~column_id (`Px_float width)
    ;;

    let get_inject t =
      get_inject'
        t
        ~get_focus:Table.Result.focus
        ~get_set_column_width:Table.Result.set_column_width
    ;;

    let get_inject_expert t =
      get_inject'
        t
        ~get_focus:Table_expert.Result.focus
        ~get_set_column_width:Table_expert.Result.set_column_width
    ;;

    let get_inject_cell_focus' t ~get_focus ~get_set_column_width =
      let focus = get_focus t in
      let module Focus_control = Table.Focus.By_cell in
      function
      | Action.Unfocus -> Focus_control.unfocus focus
      | Lock_focus -> Focus_control.lock_focus focus
      | Unlock_focus -> Focus_control.unlock_focus focus
      | Focus_down -> Focus_control.focus_down focus
      | Focus_up -> Focus_control.focus_up focus
      | Page_up -> Focus_control.page_up focus
      | Page_down -> Focus_control.page_down focus
      | Focus_cell (k, c) -> Focus_control.focus focus k (Indexed_column_id.of_int c)
      | Focus_left -> Focus_control.focus_left focus
      | Focus_right -> Focus_control.focus_right focus
      | Focus_index _ | Focus_row _ -> Effect.print_s [%message "Unsupported"]
      | Set_column_width { column_id; width } ->
        (get_set_column_width t) ~column_id (`Px_float width)
    ;;

    let get_inject_cell_focus t =
      get_inject_cell_focus'
        t
        ~get_focus:Table.Result.focus
        ~get_set_column_width:Table.Result.set_column_width
    ;;

    let summarize_focus ?num_filtered_rows (focus : int Table.Focus.By_row.optional) =
      [%message
        ""
          ~focused:(Table.Focus.By_row.focused focus : int option)
          ~num_filtered_rows:(num_filtered_rows : int option)]
      |> Sexp.to_string_hum
      |> fun s -> s ^ "\n"
    ;;

    let summarize_focus'
      ?num_filtered_rows
      (focus : (int, Indexed_column_id.t) Table.Focus.By_cell.optional)
      =
      [%message
        ""
          ~focused:
            (Table.Focus.By_cell.focused focus : (int * Indexed_column_id.t) option)
          ~num_filtered_rows:(num_filtered_rows : int option)]
      |> Sexp.to_string_hum
      |> fun s -> s ^ "\n"
    ;;

    let default
      ?theming
      ?(preload_rows = 0)
      ?(is_column_b_visible = Value.return true)
      ?override_sort
      ?default_sort
      ?multisort_columns_when
      ?(use_legacy_header = false)
      ?(row_height = Value.return (`Px 1))
      ()
      input
      filter
      =
      let module Column = Table.Columns.Dynamic_cells in
      { component =
          Table.component
            (module Int)
            ?theming
            ~focus:(By_row { on_change = focus_changed })
            ~filter
            ?override_sort
            ?default_sort
            ?multisort_columns_when
            ~row_height
            ~preload_rows
            ~columns:(columns ~use_legacy_header ~is_column_b_visible () |> Column.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_inject
      ; get_testing = Table.Result.for_testing
      ; get_focus = Table.Result.focus
      ; get_num_filtered_rows = (fun a -> Some (Table.Result.num_filtered_rows a))
      ; summarize_focus
      }
    ;;

    let default_cell_focus
      ?theming
      ?(preload_rows = 0)
      ?(is_column_b_visible = Value.return true)
      ?override_sort
      ?default_sort
      ?(use_legacy_header = false)
      ?(row_height = Value.return (`Px 1))
      ()
      input
      filter
      =
      let module Column = Table.Columns.Dynamic_cells in
      { component =
          Table.component
            (module Int)
            ?theming
            ~focus:(By_cell { on_change = focus_changed' })
            ~filter
            ?override_sort
            ?default_sort
            ~row_height
            ~preload_rows
            ~columns:(columns ~use_legacy_header ~is_column_b_visible () |> Column.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_inject = get_inject_cell_focus
      ; get_testing = Table.Result.for_testing
      ; get_focus = Table.Result.focus
      ; get_num_filtered_rows = (fun a -> Some (Table.Result.num_filtered_rows a))
      ; summarize_focus = summarize_focus'
      }
    ;;

    let default'
      ?(theming = `Themed)
      ?(with_groups = false)
      ?(preload_rows = 0)
      ?(is_column_b_visible = true)
      ()
      input
      filter
      =
      let columns =
        match with_groups with
        | false -> columns_dynamic ~is_column_b_visible
        | true -> columns_dynamic_with_groups ~is_column_b_visible
      in
      { component =
          Table.component
            (module Int)
            ~theming
            ~focus:(By_row { on_change = focus_changed })
            ~filter
            ~row_height:(Value.return (`Px 1))
            ~preload_rows
            ~columns:(Bonsai.Value.return columns |> Table.Columns.Dynamic_columns.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_testing = Table.Result.for_testing
      ; get_focus = Table.Result.focus
      ; get_inject
      ; get_num_filtered_rows = (fun a -> Some (Table.Result.num_filtered_rows a))
      ; summarize_focus
      }
    ;;

    let expert_for_testing_compute_presence
      ?(theming = `Themed)
      ~collate
      ~presence
      ()
      input
      _filter
      =
      let component =
        let%sub collation =
          Table_expert.collate
            ~filter_equal:[%compare.equal: unit]
            ~order_equal:[%compare.equal: unit]
            ~filter_to_predicate:(fun () -> None)
            ~order_to_compare:(fun () -> Unchanged)
            input
            collate
        in
        let columns =
          [ Table_expert.Columns.Dynamic_cells.column
              ~header:(Value.return (Vdom.Node.text "key"))
              ~cell:(fun ~key ~data:_ ->
                let%arr key = key in
                Vdom.Node.textf "%d" key)
              ()
          ]
          |> Table_expert.Columns.Dynamic_cells.lift
        in
        Table_expert.component
          (module Int)
          ~theming
          ~focus:
            (By_row
               { on_change = Value.return (Fn.const Effect.Ignore)
               ; compute_presence = (fun focus -> presence ~focus ~collation)
               })
          ~row_height:(Value.return (`Px 10))
          ~columns
          collation
      in
      { component
      ; get_vdom = Table_expert.Result.view
      ; get_testing = Table_expert.Result.for_testing
      ; get_focus = Table_expert.Result.focus
      ; get_inject = get_inject_expert
      ; get_num_filtered_rows = (fun _ -> None)
      ; summarize_focus
      }
    ;;
  end

  let set_bounds_helper handle ~get_vdom low_and_high =
    Handle.trigger_hook_via
      handle
      ~get_vdom
      ~selector:"div[bounds-change]"
      ~name:"bounds-change"
      Bonsai_web_ui_element_size_hooks.Visibility_tracker.For_testing.type_id
      ~f:(fun { visible_rect_changed; _ } -> visible_rect_changed)
      (Option.map low_and_high ~f:(fun (low, high) ->
         { Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.min_x = 0.0
         ; min_y = Float.of_int low
         ; max_x = 100.0
         ; max_y = Float.of_int high
         }))
  ;;

  let set_bounds_for_handle handle ~get_vdom ~low ~high =
    set_bounds_helper handle ~get_vdom (Some (low, high))
  ;;

  let set_bounds t ~low ~high =
    set_bounds_for_handle t.handle ~get_vdom:t.get_vdom ~low ~high
  ;;

  let clear_bounds_for_handle handle ~get_vdom = set_bounds_helper handle ~get_vdom None
  let clear_bounds t = clear_bounds_for_handle t.handle ~get_vdom:t.get_vdom

  let resize_column_for_handle handle ~get_vdom ~idx ~width =
    Handle.trigger_hook
      handle
      ~get_vdom
      ~selector:(sprintf "td[size_tracker]:nth-child(%d)" (idx + 1))
      ~name:"size_tracker"
      Bonsai_web_ui_element_size_hooks.Size_tracker.For_testing.type_id
      { Bonsai_web_ui_element_size_hooks.Size_tracker.For_testing.Dimensions.width
      ; height = 0.0
      }
  ;;

  let resize_column t ~idx ~width =
    resize_column_for_handle t.handle ~get_vdom:t.get_vdom ~idx ~width
  ;;
end
