open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Table_expert = Bonsai_web_ui_partial_render_table.Expert
module Columns = Table.Columns.Dynamic_cells
open Shared

let table_to_string
      ~include_stats
      (for_testing : Bonsai_web_ui_partial_render_table.For_testing.t)
  =
  let open Ascii_table_kernel in
  let module Node_h = Virtual_dom_test_helpers.Node_helpers in
  let stats =
    Ascii_table_kernel.draw
      ~limit_width_to:200
      ~prefer_split_on_spaces:false
      [ Column.create "metric" (fun (k, _) -> k)
      ; Column.create "value" (fun (_, v) -> v)
      ]
      [ "rows-before", sprintf "%d" for_testing.body.rows_before
      ; "rows-after", sprintf "%d" for_testing.body.rows_after
      ; "num-filtered", sprintf "%d" for_testing.body.num_filtered
      ; "num-unfiltered", sprintf "%d" for_testing.body.num_unfiltered
      ]
    |> Option.value_exn
    |> Ascii_table_kernel.Screen.to_string
         ~bars:`Unicode
         ~string_with_attr:(fun _attr str -> str)
  in
  let contents =
    let selected =
      Column.create
        ">"
        (fun { Bonsai_web_ui_partial_render_table.For_testing.Table_body.selected; _ } ->
           if selected then "*" else "")
    in
    let num_column =
      Column.create
        "#"
        (fun { Bonsai_web_ui_partial_render_table.For_testing.Table_body.id; _ } ->
           Int63.to_string id)
    in
    let ascii_column_of_leaf i header =
      let header = Node_h.unsafe_convert_exn header |> Node_h.inner_text in
      Column.create
        header
        (fun { Bonsai_web_ui_partial_render_table.For_testing.Table_body.view; _ } ->
           List.nth_exn view i |> Node_h.unsafe_convert_exn |> Node_h.inner_text)
    in
    let columns =
      selected
      :: num_column
      :: (for_testing.body.column_names |> List.mapi ~f:ascii_column_of_leaf)
    in
    Ascii_table_kernel.draw
      columns
      for_testing.body.cells
      ~limit_width_to:200
      ~prefer_split_on_spaces:false
    |> Option.value_exn
    |> Ascii_table_kernel.Screen.to_string
         ~bars:`Unicode
         ~string_with_attr:(fun _attr str -> str)
  in
  if include_stats then stats ^ contents else contents
;;

module Test = struct
  include Shared.Test

  let create
        (type a)
        ?(stabilize_height = true)
        ?(visible_range = 0, 100)
        ?(map = small_map)
        ?(should_set_bounds = true)
        ~stats
        component
    =
    let min_vis, max_vis = visible_range in
    let input_var = Bonsai.Var.create map in
    let filter_var = Bonsai.Var.create (fun ~key:_ ~data:_ -> true) in
    let { Component.component; get_vdom; get_testing; get_inject } =
      component (Bonsai.Var.value input_var) (Bonsai.Var.value filter_var)
    in
    let handle =
      Handle.create
        (module struct
          type t = a

          let out a = Lazy.force (get_testing a)
          let view a = table_to_string (out a) ~include_stats:stats

          type incoming = Action.t

          let incoming = get_inject
        end)
        component
    in
    let t = { handle; get_vdom; input_var; filter_var } in
    if should_set_bounds then set_bounds t ~low:min_vis ~high:max_vis;
    (* Because the component uses edge-triggering to propagate rank-range, we need to
       run the view-computers twice. *)
    if stabilize_height
    then (
      Handle.store_view handle;
      Handle.store_view handle);
    t
  ;;
end

let%expect_test "basic table" =
  let test = Test.create ~stats:true (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
┌────────────────┬───────┐
│ metric         │ value │
├────────────────┼───────┤
│ rows-before    │ 0     │
│ rows-after     │ 0     │
│ num-filtered   │ 3     │
│ num-unfiltered │ 3     │
└────────────────┴───────┘
┌───┬─────┬───────┬───────┬──────────┐
│ > │ #   │ ◇ key │ a     │ ◇ b      │
├───┼─────┼───────┼───────┼──────────┤
│   │ 0   │ 0     │ hello │ 1.000000 │
│   │ 100 │ 1     │ there │ 2.000000 │
│   │ 200 │ 4     │ world │ 2.000000 │
└───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "basic table with default sort" =
  let test =
    Test.create
      ~stats:true
      (Test.Component.default
         ~default_sort:
           (Value.return (fun (_key, { a = a1; _ }) (_key, { a = a2; _ }) ->
              -String.compare a1 a2))
         ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 4     │ world │ 2.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 0     │ hello │ 1.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "big table" =
  let test =
    Test.create
      ~stats:true
      ~map:big_map
      ~visible_range:(0, 10)
      (Test.Component.default ())
  in
  Handle.show test.handle;
  [%expect
    {|
┌────────────────┬───────┐
│ metric         │ value │
├────────────────┼───────┤
│ rows-before    │ 0     │
│ rows-after     │ 87    │
│ num-filtered   │ 99    │
│ num-unfiltered │ 99    │
└────────────────┴───────┘
┌───┬──────┬───────┬────┬──────────┐
│ > │ #    │ ◇ key │ a  │ ◇ b      │
├───┼──────┼───────┼────┼──────────┤
│   │ 0    │ 1     │ hi │ 0.000000 │
│   │ 100  │ 2     │ hi │ 1.000000 │
│   │ 200  │ 3     │ hi │ 1.000000 │
│   │ 300  │ 4     │ hi │ 2.000000 │
│   │ 400  │ 5     │ hi │ 2.000000 │
│   │ 500  │ 6     │ hi │ 3.000000 │
│   │ 600  │ 7     │ hi │ 3.000000 │
│   │ 700  │ 8     │ hi │ 4.000000 │
│   │ 800  │ 9     │ hi │ 4.000000 │
│   │ 900  │ 10    │ hi │ 5.000000 │
│   │ 1000 │ 11    │ hi │ 5.000000 │
│   │ 1100 │ 12    │ hi │ 6.000000 │
└───┴──────┴───────┴────┴──────────┘ |}]
;;

let%expect_test "table with some preload" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:true
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
┌────────────────┬───────┐
│ metric         │ value │
├────────────────┼───────┤
│ rows-before    │ 3     │
│ rows-after     │ 85    │
│ num-filtered   │ 99    │
│ num-unfiltered │ 99    │
└────────────────┴───────┘
┌───┬──────┬───────┬────┬──────────┐
│ > │ #    │ ◇ key │ a  │ ◇ b      │
├───┼──────┼───────┼────┼──────────┤
│   │ 0    │ 4     │ hi │ 2.000000 │
│   │ 100  │ 5     │ hi │ 2.000000 │
│   │ 200  │ 6     │ hi │ 3.000000 │
│   │ 300  │ 7     │ hi │ 3.000000 │
│   │ 400  │ 8     │ hi │ 4.000000 │
│   │ 500  │ 9     │ hi │ 4.000000 │
│   │ 600  │ 10    │ hi │ 5.000000 │
│   │ 700  │ 11    │ hi │ 5.000000 │
│   │ 800  │ 12    │ hi │ 6.000000 │
│   │ 900  │ 13    │ hi │ 6.000000 │
│   │ 1000 │ 14    │ hi │ 7.000000 │
└───┴──────┴───────┴────┴──────────┘ |}]
;;

let%expect_test "big table filtered" =
  let test =
    Test.create
      ~stats:true
      ~map:big_map
      ~visible_range:(0, 10)
      (Test.Component.default ())
  in
  Bonsai.Var.set test.filter_var (fun ~key ~data:_ -> key mod 2 = 0);
  Handle.show test.handle;
  [%expect
    {|
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 37    │
    │ num-filtered   │ 49    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬───────┬────┬───────────┐
    │ > │ #    │ ◇ key │ a  │ ◇ b       │
    ├───┼──────┼───────┼────┼───────────┤
    │   │ 0    │ 2     │ hi │ 1.000000  │
    │   │ 100  │ 4     │ hi │ 2.000000  │
    │   │ 200  │ 6     │ hi │ 3.000000  │
    │   │ 300  │ 8     │ hi │ 4.000000  │
    │   │ 400  │ 10    │ hi │ 5.000000  │
    │   │ 500  │ 12    │ hi │ 6.000000  │
    │   │ 600  │ 14    │ hi │ 7.000000  │
    │   │ 700  │ 16    │ hi │ 8.000000  │
    │   │ 800  │ 18    │ hi │ 9.000000  │
    │   │ 900  │ 20    │ hi │ 10.000000 │
    │   │ 1000 │ 22    │ hi │ 11.000000 │
    │   │ 1100 │ 24    │ hi │ 12.000000 │
    └───┴──────┴───────┴────┴───────────┘ |}]
;;

let%expect_test "focus down" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │ * │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "focus up" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │ * │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "unfocus" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │ * │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "remove focused moves down if possible" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Bonsai.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "focus shadow (down)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │ * │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │ * │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "focus shadow (up)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │ * │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │ * │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "remove focused causes unfocus (down)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Bonsai.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "remove focused causes unfocus (up)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ("scrolling to" (i 100) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │ * │ 100 │ 1     │ there │ 2.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Bonsai.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 0) "minimizing scrolling")
    ┌───┬─────┬───────┬───────┬──────────┐
    │ > │ #   │ ◇ key │ a     │ ◇ b      │
    ├───┼─────┼───────┼───────┼──────────┤
    │ * │ 0   │ 0     │ hello │ 1.000000 │
    │   │ 200 │ 4     │ world │ 2.000000 │
    └───┴─────┴───────┴───────┴──────────┘ |}]
;;

let%expect_test "page up" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬──────┬───────┬────┬──────────┐
    │ > │ #    │ ◇ key │ a  │ ◇ b      │
    ├───┼──────┼───────┼────┼──────────┤
    │   │ 0    │ 4     │ hi │ 2.000000 │
    │   │ 100  │ 5     │ hi │ 2.000000 │
    │   │ 200  │ 6     │ hi │ 3.000000 │
    │   │ 300  │ 7     │ hi │ 3.000000 │
    │   │ 400  │ 8     │ hi │ 4.000000 │
    │   │ 500  │ 9     │ hi │ 4.000000 │
    │   │ 600  │ 10    │ hi │ 5.000000 │
    │   │ 700  │ 11    │ hi │ 5.000000 │
    │   │ 800  │ 12    │ hi │ 6.000000 │
    │   │ 900  │ 13    │ hi │ 6.000000 │
    │   │ 1000 │ 14    │ hi │ 7.000000 │
    └───┴──────┴───────┴────┴──────────┘ |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  [%expect
    {|
    ("scrolling to" (i 200)
     "such that it is positioned at the bottom of the screen")
    ┌───┬──────┬───────┬────┬──────────┐
    │ > │ #    │ ◇ key │ a  │ ◇ b      │
    ├───┼──────┼───────┼────┼──────────┤
    │   │ 0    │ 4     │ hi │ 2.000000 │
    │   │ 100  │ 5     │ hi │ 2.000000 │
    │ * │ 200  │ 6     │ hi │ 3.000000 │
    │   │ 300  │ 7     │ hi │ 3.000000 │
    │   │ 400  │ 8     │ hi │ 4.000000 │
    │   │ 500  │ 9     │ hi │ 4.000000 │
    │   │ 600  │ 10    │ hi │ 5.000000 │
    │   │ 700  │ 11    │ hi │ 5.000000 │
    │   │ 800  │ 12    │ hi │ 6.000000 │
    │   │ 900  │ 13    │ hi │ 6.000000 │
    │   │ 1000 │ 14    │ hi │ 7.000000 │
    └───┴──────┴───────┴────┴──────────┘ |}]
;;

let%expect_test "page down" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ┌───┬──────┬───────┬────┬──────────┐
    │ > │ #    │ ◇ key │ a  │ ◇ b      │
    ├───┼──────┼───────┼────┼──────────┤
    │   │ 0    │ 4     │ hi │ 2.000000 │
    │   │ 100  │ 5     │ hi │ 2.000000 │
    │   │ 200  │ 6     │ hi │ 3.000000 │
    │   │ 300  │ 7     │ hi │ 3.000000 │
    │   │ 400  │ 8     │ hi │ 4.000000 │
    │   │ 500  │ 9     │ hi │ 4.000000 │
    │   │ 600  │ 10    │ hi │ 5.000000 │
    │   │ 700  │ 11    │ hi │ 5.000000 │
    │   │ 800  │ 12    │ hi │ 6.000000 │
    │   │ 900  │ 13    │ hi │ 6.000000 │
    │   │ 1000 │ 14    │ hi │ 7.000000 │
    └───┴──────┴───────┴────┴──────────┘ |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  [%expect
    {|
("scrolling to" (i 800)
 "such that it is positioned at the top of the screen")
┌───┬──────┬───────┬────┬──────────┐
│ > │ #    │ ◇ key │ a  │ ◇ b      │
├───┼──────┼───────┼────┼──────────┤
│   │ 0    │ 4     │ hi │ 2.000000 │
│   │ 100  │ 5     │ hi │ 2.000000 │
│   │ 200  │ 6     │ hi │ 3.000000 │
│   │ 300  │ 7     │ hi │ 3.000000 │
│   │ 400  │ 8     │ hi │ 4.000000 │
│   │ 500  │ 9     │ hi │ 4.000000 │
│   │ 600  │ 10    │ hi │ 5.000000 │
│   │ 700  │ 11    │ hi │ 5.000000 │
│ * │ 800  │ 12    │ hi │ 6.000000 │
│   │ 900  │ 13    │ hi │ 6.000000 │
│   │ 1000 │ 14    │ hi │ 7.000000 │
└───┴──────┴───────┴────┴──────────┘ |}]
;;

let%expect_test "actions on empty table" =
  let test =
    Test.create
      ~map:(Map.empty (module Int))
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  (* just make sure nothing weird happens *)
  Handle.do_actions test.handle [ Page_down; Page_up; Focus_down; Focus_up; Unfocus ]
;;
