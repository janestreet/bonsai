open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
open! Bonsai_web_ui_partial_render_table
open! Bonsai_web_ui_partial_render_table_bench
open! Bonsai_web_ui_partial_render_table_test.Shared_with_bench
open! Incr_map_collate

module Config = struct
  type ('data, 'column_id) t =
    { name : string
    ; columns : (int, 'data, 'column_id) Expert.Columns.t
    ; first_column : 'column_id
    }

  let dynamic_cells =
    { name = "Dynamic_cells"
    ; columns = Dynamic_cells.all
    ; first_column = Dynamic_cells.first_column
    }
  ;;

  let dynamic_columns =
    { name = "Dynamic_columns"
    ; columns = Dynamic_columns.all
    ; first_column = Dynamic_columns.first_column
    }
  ;;

  let dynamic_experimental =
    { name = "Dynamic_experimental"
    ; columns = Dynamic_experimental.all
    ; first_column = Dynamic_experimental.first_column
    }
  ;;

  let create
    { name; columns; first_column = _ }
    key_cmp
    ~test_name
    ~initial_vars
    ~interaction
    =
    create_bench
      key_cmp
      ~columns
      ~test_name:[%string "%{name}: %{test_name}"]
      ~initial_vars
      ~interaction
  ;;
end

let focus_and_unfocus ~(config : _ Config.t) ~size ~in_range =
  let starting_map = Row.init_rows size in
  let not_ = if in_range then "" else "not " in
  let column = config.first_column in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:
      [%string "Focus by key (key %{not_}present) and unfocus in %{size#Int} element map"]
    ~interaction:(fun _ ->
      let index = if in_range then 1 else size + 1 in
      [ Interaction.inject (Action.Focus (index, column))
      ; Interaction.inject Action.Unfocus
      ; Interaction.reset_model
      ]
      |> Interaction.many_with_stabilizations)
;;

let focus_up_and_down ~config ~size =
  let starting_map = Row.init_rows size in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:[%string "Focus up and down in %{size#Int} element map"]
    ~interaction:(fun _ ->
      [ Interaction.inject Action.Focus_down; Interaction.inject Action.Focus_up ]
      |> Interaction.many_with_stabilizations)
;;

(* It doesn't make a lot of sense to have a really large number of columns for left/right
   focus benchmarks, so we just do it within the same size table as everything else which
   seems more realistic. *)
let focus_left_and_right ~config ~num_rows =
  let starting_map = Row.init_rows num_rows in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:[%string "Focus left and right in a map with %{num_rows#Int} rows"]
    ~interaction:(fun _ ->
      [ Interaction.inject Action.Focus_left; Interaction.inject Action.Focus_right ]
      |> Interaction.many_with_stabilizations)
;;

let page_up_and_down ~config ~size =
  let starting_map = Row.init_rows size in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:[%string "Page up and down in %{size#Int} element map"]
    ~interaction:(fun _ ->
      [ Interaction.inject Action.Page_down; Interaction.inject Action.Page_up ]
      |> Interaction.many_with_stabilizations)
;;

let scroll ~config ~size ~start ~stop ~window_size =
  let starting_map = Row.init_rows size in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:
      [%string
        "Scroll %{window_size#Int}-wide window from %{start#Int} to %{stop#Int} and back \
         in %{size#Int} element map"]
    ~interaction:(fun input ->
      [ Input.scroll input ~start ~stop ~window_size
      ; Input.scroll input ~start:stop ~stop:start ~window_size
      ]
      |> Interaction.many_with_stabilizations)
;;

let apply_filters ~config ~size ~window_size =
  let starting_map = Row.init_rows size in
  Config.create
    config
    (module Int)
    ~initial_vars:
      (Input.create ~rank_range:(Collate.Which_range.To (window_size - 1)) starting_map)
    ~test_name:
      [%string
        "Apply 4 filters and clear with %{size#Int} element map using %{window_size#Int} \
         window"]
    ~interaction:(fun input ->
      [ Input.apply_filter input (fun ~key ~data:_ -> key mod 2 = 0)
      ; Input.apply_filter input (fun ~key ~data:_ -> key mod 3 = 0)
      ; Input.apply_filter input (fun ~key ~data:_ -> key mod 4 = 0)
      ; Input.apply_filter input (fun ~key ~data:_ -> key mod 5 = 0)
      ; Input.clear_filter input
      ]
      |> Interaction.many_with_stabilizations)
;;

let invert_ordering ~config ~size =
  let starting_map = Row.init_rows size in
  Config.create
    config
    (module Int)
    ~initial_vars:(Input.create starting_map)
    ~test_name:[%string "Invert ordering of %{size#Int} element map twice"]
    ~interaction:(fun input ->
      [ Input.set_order
          input
          (Compare.Custom_by_key_and_value
             { compare = (fun (_, a) (_, b) -> [%compare: int] a.Row.asize b.Row.asize) })
      ; Input.set_order
          input
          (Compare.Custom_by_key_and_value
             { compare = (fun (_, a) (_, b) -> [%compare: int] b.Row.asize a.Row.asize) })
      ]
      |> Interaction.many_with_stabilizations)
;;

(* [set_map] sets performs [num_sets * batch_size] map sets in total, with stabilization
   happening every [batch_size] changes. [window_size] specifies the size of the window,
   and the sets wrap around it. *)
let set_map ~config ~size ~num_sets ~batch_size ~window_size =
  let starting_map = Row.init_rows size in
  let current_map = ref starting_map in
  Config.create
    config
    (module Int)
    ~initial_vars:
      (Input.create ~rank_range:(Collate.Which_range.To (window_size - 1)) starting_map)
    ~test_name:
      [%string
        "Perform %{num_sets#Int} sets of %{batch_size#Int} items in a %{size#Int} \
         element map with %{window_size#Int}-wide window "]
    ~interaction:(fun input ->
      [ List.init num_sets ~f:(fun set_num ->
          List.init batch_size ~f:(fun i ->
            let index = (set_num * batch_size) + i in
            current_map
              := Map.set
                   !current_map
                   ~key:(index mod window_size)
                   ~data:(Row.of_int (index + size));
            Input.set_map input !current_map)
          |> Interaction.many)
        |> Interaction.many_with_stabilizations
      ; Input.set_map input starting_map
      ]
      |> Interaction.many_with_stabilizations)
;;

let benchmarks_for config =
  [ focus_and_unfocus ~config ~size:10 ~in_range:false
  ; focus_and_unfocus ~config ~size:100 ~in_range:false
  ; focus_and_unfocus ~config ~size:101 ~in_range:false
  ; focus_and_unfocus ~config ~size:1000 ~in_range:false
  ; focus_and_unfocus ~config ~size:10000 ~in_range:false
  ; focus_and_unfocus ~config ~size:10 ~in_range:true
  ; focus_and_unfocus ~config ~size:100 ~in_range:true
  ; focus_and_unfocus ~config ~size:101 ~in_range:true
  ; focus_and_unfocus ~config ~size:1000 ~in_range:true
  ; focus_and_unfocus ~config ~size:10000 ~in_range:true
  ; focus_up_and_down ~config ~size:10
  ; focus_up_and_down ~config ~size:100
  ; focus_up_and_down ~config ~size:101
  ; focus_up_and_down ~config ~size:1000
  ; focus_up_and_down ~config ~size:10000
  ; focus_left_and_right ~config ~num_rows:10
  ; focus_left_and_right ~config ~num_rows:100
  ; focus_left_and_right ~config ~num_rows:101
  ; focus_left_and_right ~config ~num_rows:1000
  ; focus_left_and_right ~config ~num_rows:10000
  ; page_up_and_down ~config ~size:10
  ; focus_up_and_down ~config ~size:10
  ; focus_up_and_down ~config ~size:100
  ; focus_up_and_down ~config ~size:101
  ; focus_up_and_down ~config ~size:1000
  ; focus_up_and_down ~config ~size:10000
  ; page_up_and_down ~config ~size:10
  ; page_up_and_down ~config ~size:100
  ; page_up_and_down ~config ~size:101
  ; page_up_and_down ~config ~size:1000
  ; page_up_and_down ~config ~size:10000
  ; scroll ~config ~size:100 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~config ~size:100 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~config ~size:1000 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~config ~size:1000 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~config ~size:1000 ~start:0 ~stop:9 ~window_size:100
  ; apply_filters ~config ~size:100 ~window_size:10
  ; apply_filters ~config ~size:101 ~window_size:10
  ; apply_filters ~config ~size:1000 ~window_size:10
  ; apply_filters ~config ~size:1000 ~window_size:50
  ; apply_filters ~config ~size:10000 ~window_size:50
  ; apply_filters ~config ~size:10000 ~window_size:100
  ; invert_ordering ~config ~size:10
  ; invert_ordering ~config ~size:100
  ; invert_ordering ~config ~size:101
  ; invert_ordering ~config ~size:1000
  ; set_map ~config ~size:10 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~config ~size:10 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~config ~size:11 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~config ~size:11 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~config ~size:100 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~config ~size:100 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~config ~size:1000 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~config ~size:1000 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~config ~size:1000 ~num_sets:10 ~batch_size:10 ~window_size:100
  ]
;;

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    (benchmarks_for Config.dynamic_columns
     @ benchmarks_for Config.dynamic_cells
     @ benchmarks_for Config.dynamic_experimental)
;;
