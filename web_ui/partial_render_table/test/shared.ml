open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Table_expert = Bonsai_web_ui_partial_render_table.Expert
module Columns = Table.Columns.Dynamic_cells

module Action = struct
  type t =
    | Unfocus
    | Focus_down
    | Focus_up
    | Page_up
    | Page_down
    | Focus of int
    | Focus_index of int
end

type t =
  { a : string
  ; b : float
  ; c : string
  ; d : int
  ; e : string
  }
[@@deriving fields]

let columns ~is_column_b_visible =
  [ Columns.column
      ~label:(Value.return (Vdom.Node.text "key"))
      ~sort:
        (Value.return (fun a b ->
           Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b))
      ~cell:(fun ~key ~data:_ ->
        let%arr key = key in
        Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~label:(Value.return (Vdom.Node.text "a"))
      ~cell:(fun ~key:_ ~data ->
        let%sub state = Bonsai.state (module String) ~default_model:"" in
        let%arr { a; _ } = data
        and state, set_state = state in
        Vdom.Node.div
          [ Vdom.Node.input ~attrs:[ Vdom.Attr.on_input (fun _ -> set_state) ] ()
          ; Vdom.Node.textf "%s %s" a state
          ])
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~label:(Value.return (Vdom.Node.text "b"))
      ~sort:
        (Value.return (fun a b_1 ->
           Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b) a b_1))
      ~cell:(fun ~key:_ ~data ->
        let%arr { b; _ } = data in
        Vdom.Node.textf "%f" b)
      ()
  ]
;;

let columns_dynamic ~is_column_b_visible =
  let module Columns = Table.Columns.Dynamic_columns in
  [ Columns.column
      ~label:(Vdom.Node.text "key")
      ~sort:(fun a b -> Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b)
      ~cell:(fun ~key ~data:_ -> Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~label:(Vdom.Node.text "a")
      ~sort:(fun a_1 b ->
        Comparable.lift [%compare: string] ~f:(fun (_, { a; _ }) -> a) a_1 b)
      ~cell:(fun ~key:_ ~data:{ a; _ } -> Vdom.Node.textf "%s" a)
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~label:(Vdom.Node.text "b")
      ~sort:(fun a b_1 ->
        Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b) a b_1)
      ~cell:(fun ~key:_ ~data:{ b; _ } -> Vdom.Node.textf "%f" b)
      ()
  ]
;;

let columns_dynamic_with_groups ~is_column_b_visible =
  let module Columns = Table.Columns.Dynamic_columns in
  let cols =
    [ Columns.column
        ~label:(Vdom.Node.text "key")
        ~sort:(fun a b -> Comparable.lift [%compare: int] ~f:(fun (key, _) -> key) a b)
        ~cell:(fun ~key ~data:_ -> Vdom.Node.textf "%d" key)
        ()
    ]
  in
  let group_1 =
    Columns.group
      ~label:(Vdom.Node.text "Basics")
      [ Columns.column
          ~label:(Vdom.Node.text "a")
          ~sort:(fun a_1 b ->
            Comparable.lift [%compare: string] ~f:(fun (_, { a; _ }) -> a) a_1 b)
          ~cell:(fun ~key:_ ~data:{ a; _ } -> Vdom.Node.textf "%s" a)
          ()
      ; Columns.column
          ~visible:is_column_b_visible
          ~label:(Vdom.Node.text "b")
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
          ~label:(Vdom.Node.text "c")
          ~sort:(fun a_1 b ->
            Comparable.lift [%compare: string] ~f:(fun (_, { c; _ }) -> c) a_1 b)
          ~cell:(fun ~key:_ ~data:{ c; _ } -> Vdom.Node.textf "%s" c)
          ()
      ; Columns.column
          ~visible:is_column_b_visible
          ~label:(Vdom.Node.text "d")
          ~sort:(fun a b_1 ->
            Comparable.lift [%compare: int] ~f:(fun (_, { d; _ }) -> d) a b_1)
          ~cell:(fun ~key:_ ~data:{ d; _ } -> Vdom.Node.textf "%d" d)
          ()
      ]
  in
  let group_2_nested =
    Columns.group
      ~label:(Vdom.Node.text "Level 1")
      [ level_2
      ; Columns.column
          ~label:(Vdom.Node.text "e")
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
    [ 0, { a = "hello"; b = 1.0; c = "c"; d = 1; e = "x" }
    ; 1, { a = "there"; b = 2.0; c = "c"; d = 2; e = "y" }
    ; 4, { a = "world"; b = 2.0; c = "c"; d = 3; e = "z" }
    ]
;;

let big_map =
  Int.Map.of_alist_exn
    (List.range 1 100
     |> List.map ~f:(fun i ->
       i, { a = "hi"; b = Float.of_int (i / 2); c = "apple"; d = 100; e = "1st" }))
;;

let groups_map =
  Int.Map.of_alist_exn
    [ 0, { a = "hello"; b = 1.0; c = "apple"; d = 100; e = "1st" }
    ; 1, { a = "there"; b = 2.0; c = "banana"; d = 100; e = "3rd" }
    ; 4, { a = "world"; b = 2.0; c = "pear"; d = 200; e = "2nd" }
    ]
;;

module Test = struct
  type outer = t

  type 'a t =
    { handle : ('a, Action.t) Bonsai_web_test.Handle.t
    ; get_vdom : 'a -> Vdom.Node.t
    ; get_num_filtered_rows : 'a -> int option
    ; get_focus : 'a -> int Table.Focus.By_row.optional
    ; input_var : outer Int.Map.t Bonsai.Var.t
    ; filter_var : (key:int -> data:outer -> bool) Bonsai.Var.t
    }

  let focus_changed =
    Value.return (fun focus_changed_to ->
      Effect.print_s [%message (focus_changed_to : int option)])
  ;;

  module Component = struct
    type 'a t =
      { component : 'a Computation.t
      ; get_vdom : 'a -> Vdom.Node.t
      ; get_inject : 'a -> Action.t -> unit Ui_effect.t
      ; get_testing : 'a -> Bonsai_web_ui_partial_render_table.For_testing.t Lazy.t
      ; get_focus : 'a -> int Table.Focus.By_row.optional
      ; get_num_filtered_rows : 'a -> int option
      }

    let get_inject' t f =
      let focus : _ Table.Focus.By_row.t = f t in
      function
      | Action.Unfocus -> focus.unfocus
      | Focus_down -> focus.focus_down
      | Focus_up -> focus.focus_up
      | Page_up -> focus.page_up
      | Page_down -> focus.page_down
      | Focus k -> focus.focus k
      | Focus_index index -> focus.focus_index index
    ;;

    let get_inject t = get_inject' t Table.Result.focus
    let get_inject_expert t = get_inject' t Table_expert.Result.focus

    let default
          ?(preload_rows = 0)
          ?(is_column_b_visible = Value.return true)
          ?default_sort
          ()
          input
          filter
      =
      { component =
          Table.component
            (module Int)
            ~focus:(By_row { on_change = focus_changed })
            ~filter
            ?default_sort
            ~row_height:(`Px 1)
            ~preload_rows
            ~columns:(columns ~is_column_b_visible |> Columns.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_inject
      ; get_testing = Table.Result.for_testing
      ; get_focus = Table.Result.focus
      ; get_num_filtered_rows = (fun a -> Some (Table.Result.num_filtered_rows a))
      }
    ;;

    let default'
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
            ~focus:(By_row { on_change = focus_changed })
            ~filter
            ~row_height:(`Px 1)
            ~preload_rows
            ~columns:(Bonsai.Value.return columns |> Table.Columns.Dynamic_columns.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_testing = Table.Result.for_testing
      ; get_focus = Table.Result.focus
      ; get_inject
      ; get_num_filtered_rows = (fun a -> Some (Table.Result.num_filtered_rows a))
      }
    ;;

    let expert_for_testing_compute_presence ~collate ~presence () input _filter =
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
              ~label:(Value.return (Vdom.Node.text "key"))
              ~cell:(fun ~key ~data:_ ->
                let%arr key = key in
                Vdom.Node.textf "%d" key)
              ()
          ]
          |> Table_expert.Columns.Dynamic_cells.lift
        in
        Table_expert.component
          (module Int)
          ~focus:
            (By_row
               { on_change = Value.return (Fn.const Effect.Ignore)
               ; compute_presence = (fun focus -> presence ~focus ~collation)
               })
          ~row_height:(`Px 10)
          ~columns
          collation
      in
      { component
      ; get_vdom = Table_expert.Result.view
      ; get_testing = Table_expert.Result.for_testing
      ; get_focus = Table_expert.Result.focus
      ; get_inject = get_inject_expert
      ; get_num_filtered_rows = (fun _ -> None)
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
         { Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.min_x = 0
         ; min_y = low
         ; max_x = 100
         ; max_y = high
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
