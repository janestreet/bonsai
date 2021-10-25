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
end

type t =
  { a : string
  ; b : float
  }
[@@deriving fields]

let columns ~is_column_b_visible =
  [ Columns.column
      ~label:(Value.return (Vdom.Node.text "key"))
      ~sort:(Value.return (Comparable.lift [%compare: int] ~f:(fun (key, _) -> key)))
      ~cell:(fun ~key ~data:_ ->
        return
        @@ let%map key = key in
        Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~label:(Value.return (Vdom.Node.text "a"))
      ~cell:(fun ~key:_ ~data ->
        let%sub state = Bonsai.state [%here] (module String) ~default_model:"" in
        return
        @@ let%map { a; _ } = data
        and state, set_state = state in
        Vdom.Node.div
          [ Vdom.Node.input ~attr:(Vdom.Attr.on_input (fun _ -> set_state)) []
          ; Vdom.Node.textf "%s %s" a state
          ])
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~label:(Value.return (Vdom.Node.text "b"))
      ~sort:(Value.return (Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b)))
      ~cell:(fun ~key:_ ~data ->
        return
        @@ let%map { b; _ } = data in
        Vdom.Node.textf "%f" b)
      ()
  ]
;;

let columns_dynamic ~is_column_b_visible =
  let module Columns = Table.Columns.Dynamic_columns in
  [ Columns.column
      ~label:(Vdom.Node.text "key")
      ~sort:(Comparable.lift [%compare: int] ~f:(fun (key, _) -> key))
      ~cell:(fun ~key ~data:_ -> Vdom.Node.textf "%d" key)
      ()
  ; Columns.column
      ~label:(Vdom.Node.text "a")
      ~sort:(Comparable.lift [%compare: string] ~f:(fun (_, { a; _ }) -> a))
      ~cell:(fun ~key:_ ~data:{ a; _ } -> Vdom.Node.textf "%s" a)
      ()
  ; Columns.column
      ~visible:is_column_b_visible
      ~label:(Vdom.Node.text "b")
      ~sort:(Comparable.lift [%compare: float] ~f:(fun (_, { b; _ }) -> b))
      ~cell:(fun ~key:_ ~data:{ b; _ } -> Vdom.Node.textf "%f" b)
      ()
  ]
;;

let small_map =
  Int.Map.of_alist_exn
    [ 0, { a = "hello"; b = 1.0 }
    ; 1, { a = "there"; b = 2.0 }
    ; 4, { a = "world"; b = 2.0 }
    ]
;;

let big_map =
  Int.Map.of_alist_exn
    (List.range 1 100 |> List.map ~f:(fun i -> i, { a = "hi"; b = Float.of_int (i / 2) }))
;;

module Test = struct
  type outer = t

  type 'a t =
    { handle : ('a, Action.t) Bonsai_test.Handle.t
    ; get_vdom : 'a -> Vdom.Node.t
    ; input_var : outer Int.Map.t Bonsai.Var.t
    ; filter_var : (key:int -> data:outer -> bool) Bonsai.Var.t
    }

  module Component = struct
    type 'a t =
      { component : 'a Computation.t
      ; get_vdom : 'a -> Vdom.Node.t
      ; get_inject : 'a -> Action.t -> unit Ui_effect.t
      ; get_testing : 'a -> Bonsai_web_ui_partial_render_table.For_testing.t Lazy.t
      }

    let get_inject' t f =
      let focus : _ Bonsai_web_ui_partial_render_table.Focus.By_row.t = f t in
      function
      | Action.Unfocus -> focus.unfocus
      | Focus_down -> focus.focus_down
      | Focus_up -> focus.focus_up
      | Page_up -> focus.page_up
      | Page_down -> focus.page_down
      | Focus k -> focus.focus k
    ;;

    let get_inject t = get_inject' t Table.Result.focus

    let get_inject_expert t =
      get_inject' t Bonsai_web_ui_partial_render_table.Expert.Result.focus
    ;;

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
            ~focus:By_row
            ~filter
            ?default_sort
            ~row_height:(`Px 1)
            ~preload_rows
            ~columns:(columns ~is_column_b_visible |> Columns.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_inject
      ; get_testing = Table.Result.for_testing
      }
    ;;

    let default' ?(preload_rows = 0) ?(is_column_b_visible = true) () input filter =
      { component =
          Table.component
            (module Int)
            ~focus:By_row
            ~filter
            ~row_height:(`Px 1)
            ~preload_rows
            ~columns:
              (Bonsai.Value.return (columns_dynamic ~is_column_b_visible)
               |> Table.Columns.Dynamic_columns.lift)
            input
      ; get_vdom = Table.Result.view
      ; get_testing = Table.Result.for_testing
      ; get_inject
      }
    ;;
  end

  let set_bounds t ~low ~high =
    Handle.trigger_hook
      t.handle
      ~get_vdom:t.get_vdom
      ~selector:"div[bounds-change]"
      ~name:"bounds-change"
      Bonsai_web_ui_element_size_hooks.Visibility_tracker.For_testing.type_id
      { Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bounds.min_x = 0
      ; min_y = low
      ; max_x = 100
      ; max_y = high
      }
  ;;

  let resize_column t ~idx ~width =
    Handle.trigger_hook
      t.handle
      ~get_vdom:t.get_vdom
      ~selector:(sprintf "td[size_tracker]:nth-child(%d)" (idx + 1))
      ~name:"size_tracker"
      Bonsai_web_ui_element_size_hooks.Size_tracker.For_testing.type_id
      { Bonsai_web_ui_element_size_hooks.Size_tracker.For_testing.Dimensions.width
      ; height = 0.0
      }
  ;;
end
