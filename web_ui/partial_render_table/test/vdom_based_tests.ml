open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Table_expert = Bonsai_web_ui_partial_render_table.Expert
module Columns = Table.Columns.Dynamic_cells
open Shared

module Test = struct
  include Shared.Test

  let create
        (type a)
        ?(visible_range = 0, 100)
        ?(map = small_map)
        ?(should_print_styles = false)
        ?(should_set_bounds = true)
        component
    : a t
    =
    let min_vis, max_vis = visible_range in
    let input_var = Bonsai.Var.create map in
    let filter_var = Bonsai.Var.create (fun ~key:_ ~data:_ -> true) in
    let { Component.component; get_vdom; get_testing = _; get_inject } =
      component (Bonsai.Var.value input_var) (Bonsai.Var.value filter_var)
    in
    let handle =
      Handle.create
        (module struct
          type t = a

          let view result =
            result
            |> get_vdom
            |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
            |> Virtual_dom_test_helpers.Node_helpers.to_string_html
                 ~filter_printed_attributes:(function
                   | x when String.is_prefix ~prefix:"style." x -> should_print_styles
                   | _ -> true)
          ;;

          type incoming = Action.t

          let incoming = get_inject
        end)
        component
    in
    let t = { handle; get_vdom; input_var; filter_var } in
    if should_set_bounds then set_bounds t ~low:min_vis ~high:max_vis;
    t
  ;;

  let print_message_on_result_recomputation t =
    let result = Incr.map (Handle.result_incr t.handle) ~f:t.get_vdom in
    Incr.Observer.on_update_exn (Incr.observe result) ~f:(function
      | Initialized _ -> print_endline "Initialized"
      | Changed _ -> print_endline "Changed"
      | Invalidated -> assert false)
  ;;
end

let print_assocs component =
  let rec count needle = function
    | Sexp.Atom s when String.equal needle s -> 1
    | Atom _ -> 0
    | List l -> List.sum (module Int) l ~f:(count needle)
  in
  let structure =
    component
    |> Bonsai.Private.reveal_computation
    |> Bonsai.Private.Computation.sexp_of_packed
  in
  let assoc_count = count "Assoc" structure in
  let assoc_simple_count = count "Assoc_simpl" structure in
  print_s [%message (assoc_count : int) (assoc_simple_count : int)]
;;

let%expect_test "simplified_assocs" =
  let { Test.Component.component; _ } =
    Test.Component.default
      ()
      (Bonsai.Value.return Int.Map.empty)
      (Bonsai.Value.return (fun ~key:_ ~data:_ -> true))
  in
  print_assocs component;
  (* there's only one assoc because all the columns are inside of an assoc
     per-row instead of it being the other way around as you might have
     expected. *)
  [%expect {| ((assoc_count 1) (assoc_simple_count 2)) |}]
;;

let%expect_test "simplified_assocs on the dynamic columns" =
  let { Test.Component.component; _ } =
    Test.Component.default'
      ()
      (Bonsai.Value.return Int.Map.empty)
      (Bonsai.Value.return (fun ~key:_ ~data:_ -> true))
  in
  print_assocs component;
  (* No assocs here because it just uses the Incr_map function directly *)
  [%expect {| ((assoc_count 0) (assoc_simple_count 0)) |}]
;;

let%expect_test "column visibility" =
  let is_column_b_visible_var = Bonsai.Var.create true in
  let is_column_b_visible = Bonsai.Var.value is_column_b_visible_var in
  let test =
    Test.create ~should_print_styles:true (Test.Component.default ~is_column_b_visible ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Bonsai.Var.set is_column_b_visible_var false;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect
    {|
                resize: horizontal;
                overflow: hidden;
                width: 50px;
              }>
            <div> a </div>
          </td>
          <td colspan="1"
              freeze_width=((set <fun>)(reset <fun>))
              size_tracker=<fun>
              style={
                text-align: center;
                user-select: none;
                font-weight: bold;
                resize: horizontal;
                overflow: hidden;
                width: 50px;
+|              display: none;
              }>
            <div onclick style={ white-space: pre; cursor: pointer; }>
              <span> ◇  b </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    <div bounds-change=<fun> style={ height: 3px; position: relative; }>
      <div @key=0
           class="prt-table-row prt-table-row-even"
           onclick
           style={
             top: 0px;
             position: absolute;
             max-height: 1px;

               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }>
          <input oninput> </input>
          hello
        </div>
        <div @key=key_0-2
             data-row-id="key_0"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
-|             display: inline-block;
+|             display: none;
               contain: strict;
-|             width: 0.00px;
-|             min-width: 0.00px;
-|             max-width: 0.00px;
-|           }> 1.000000 </div>
+|           }> </div>
      </div>
      <div @key=100
           class="prt-table-row prt-table-row-odd"
           onclick
           style={
             top: 1px;
             position: absolute;
             max-height: 1px;
             width: max-content;
           }>
        <div @key=key_100-0
             data-row-id="key_100"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;

               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }>
          <input oninput> </input>
          there
        </div>
        <div @key=key_100-2
             data-row-id="key_100"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
-|             display: inline-block;
+|             display: none;
               contain: strict;
-|             width: 0.00px;
-|             min-width: 0.00px;
-|             max-width: 0.00px;
-|           }> 2.000000 </div>
+|           }> </div>
      </div>
      <div @key=200
           class="prt-table-row prt-table-row-even"
           onclick
           style={
             top: 2px;
             position: absolute;
             max-height: 1px;
             width: max-content;
           }>
        <div @key=key_200-0
             data-row-id="key_200"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;

               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }>
          <input oninput> </input>
          world
        </div>
        <div @key=key_200-2
             data-row-id="key_200"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
-|             display: inline-block;
+|             display: none;
               contain: strict;
-|             width: 0.00px;
-|             min-width: 0.00px;
-|             max-width: 0.00px;
-|           }> 2.000000 </div>
+|           }> </div>
      </div>
    </div>
  </div> |}]
;;

let%expect_test "stabilization of view range" =
  let test =
    Test.create (Test.Component.default ()) ~visible_range:(0, 2) ~should_set_bounds:false
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
<div class="partial-render-table-bonsai_path_replaced_in_test">
  <table class="prt-table-header" size_tracker=<fun>>
    <tbody>
      <tr>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  key </span>
          </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div> a </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  b </span>
          </div>
        </td>
      </tr>
    </tbody>
  </table>
  <div bounds-change=<fun>>
    <div @key=0 class="prt-table-row prt-table-row-even" onclick>
      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 0 </div>
      <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
        <input oninput> </input>
        hello
      </div>
      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 1.000000 </div>
    </div>
    <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 1 </div>
      <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
        <input oninput> </input>
        there
      </div>
      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 2.000000 </div>
    </div>
  </div>
</div> |}];
  (* Change the visibility to show the rest of the nodes *)
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect {||}];
  Test.set_bounds test ~low:0 ~high:100;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect
    {|
      <div @key=0 class="prt-table-row prt-table-row-even" onclick>
        <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 0 </div>
        <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
          <input oninput> </input>
          hello
        </div>
        <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 1.000000 </div>
      </div>
      <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
        <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 1 </div>
        <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
          <input oninput> </input>
          there
        </div>
        <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 2.000000 </div>
      </div>
+|    <div @key=200 class="prt-table-row prt-table-row-even" onclick>
+|      <div @key=key_200-0 data-row-id="key_200" class="prt-table-cell"> 4 </div>
+|      <div @key=key_200-1 data-row-id="key_200" class="prt-table-cell">
+|        <input oninput> </input>
+|        world
+|      </div>
+|      <div @key=key_200-2 data-row-id="key_200" class="prt-table-cell"> 2.000000 </div>
+|    </div>
    </div>
  </div> |}]
;;

let%expect_test "resize-column" =
  let test = Test.create ~should_print_styles:true (Test.Component.default ()) in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Test.resize_column test ~idx:0 ~width:10.0;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect
    {|
             top: 0px;
             position: absolute;
             max-height: 1px;
             width: max-content;
           }>
        <div @key=key_0-0
             data-row-id="key_0"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
-|             width: 0.00px;
+|             width: 10.00px;
-|             min-width: 0.00px;
+|             min-width: 10.00px;
-|             max-width: 0.00px;
+|             max-width: 10.00px;
             }> 0 </div>
        <div @key=key_0-1
             data-row-id="key_0"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }>

             top: 1px;
             position: absolute;
             max-height: 1px;
             width: max-content;
           }>
        <div @key=key_100-0
             data-row-id="key_100"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
-|             width: 0.00px;
+|             width: 10.00px;
-|             min-width: 0.00px;
+|             min-width: 10.00px;
-|             max-width: 0.00px;
+|             max-width: 10.00px;
             }> 1 </div>
        <div @key=key_100-1
             data-row-id="key_100"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }>

             top: 2px;
             position: absolute;
             max-height: 1px;
             width: max-content;
           }>
        <div @key=key_200-0
             data-row-id="key_200"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
-|             width: 0.00px;
+|             width: 10.00px;
-|             min-width: 0.00px;
+|             min-width: 10.00px;
-|             max-width: 0.00px;
+|             max-width: 10.00px;
             }> 4 </div>
        <div @key=key_200-1
             data-row-id="key_200"
             class="prt-table-cell"
             style={
               height: 1px;
               min-height: 1px;
               max-height: 1px;
               box-sizing: border-box;
               overflow: hidden;
               display: inline-block;
               contain: strict;
               width: 0.00px;
               min-width: 0.00px;
               max-width: 0.00px;
             }> |}]
;;

let%expect_test "big table" =
  (* The PRT always renders [low-25, high+25], so 50,50 will render a big chunk
     centered at 50 *)
  let test =
    Test.create ~map:big_map ~visible_range:(50, 50) (Test.Component.default ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
<div class="partial-render-table-bonsai_path_replaced_in_test">
  <table class="prt-table-header" size_tracker=<fun>>
    <tbody>
      <tr>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  key </span>
          </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div> a </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  b </span>
          </div>
        </td>
      </tr>
    </tbody>
  </table>
  <div bounds-change=<fun>>
    <div @key=0 class="prt-table-row prt-table-row-even" onclick>
      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 51 </div>
      <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
        <input oninput> </input>
        hi
      </div>
      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 25.000000 </div>
    </div>
    <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 52 </div>
      <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
        <input oninput> </input>
        hi
      </div>
      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 26.000000 </div>
    </div>
  </div>
</div> |}];
  (* extending the range upwards should only add to the end *)
  Test.set_bounds test ~low:55 ~high:60;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect
    {|
            <div onclick>
              <span> ◇  key </span>
            </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div> a </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
              <span> ◇  b </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    <div bounds-change=<fun>>
-|    <div @key=0 class="prt-table-row prt-table-row-even" onclick>
+|    <div @key=0 class="prt-table-row prt-table-row-odd" onclick>
-|      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 51 </div>
+|      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 56 </div>
        <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
          <input oninput> </input>
          hi
        </div>
-|      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 25.000000 </div>
+|      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 28.000000 </div>
      </div>
-|    <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
+|    <div @key=100 class="prt-table-row prt-table-row-even" onclick>
-|      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 52 </div>
+|      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 57 </div>
        <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
          <input oninput> </input>
          hi
        </div>
-|      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 26.000000 </div>
+|      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 28.000000 </div>
+|    </div>
+|    <div @key=200 class="prt-table-row prt-table-row-odd" onclick>
+|      <div @key=key_200-0 data-row-id="key_200" class="prt-table-cell"> 58 </div>
+|      <div @key=key_200-1 data-row-id="key_200" class="prt-table-cell">
+|        <input oninput> </input>
+|        hi
+|      </div>
+|      <div @key=key_200-2 data-row-id="key_200" class="prt-table-cell"> 29.000000 </div>
+|    </div>
+|    <div @key=300 class="prt-table-row prt-table-row-even" onclick>
+|      <div @key=key_300-0 data-row-id="key_300" class="prt-table-cell"> 59 </div>
+|      <div @key=key_300-1 data-row-id="key_300" class="prt-table-cell">
+|        <input oninput> </input>
+|        hi
+|      </div>
+|      <div @key=key_300-2 data-row-id="key_300" class="prt-table-cell"> 29.000000 </div>
+|    </div>
+|    <div @key=400 class="prt-table-row prt-table-row-odd" onclick>
+|      <div @key=key_400-0 data-row-id="key_400" class="prt-table-cell"> 60 </div>
+|      <div @key=key_400-1 data-row-id="key_400" class="prt-table-cell">
+|        <input oninput> </input>
+|        hi
+|      </div>
+|      <div @key=key_400-2 data-row-id="key_400" class="prt-table-cell"> 30.000000 </div>
+|    </div>
+|    <div @key=500 class="prt-table-row prt-table-row-even" onclick>
+|      <div @key=key_500-0 data-row-id="key_500" class="prt-table-cell"> 61 </div>
+|      <div @key=key_500-1 data-row-id="key_500" class="prt-table-cell">
+|        <input oninput> </input>
+|        hi
+|      </div>
+|      <div @key=key_500-2 data-row-id="key_500" class="prt-table-cell"> 30.000000 </div>
+|    </div>
+|    <div @key=600 class="prt-table-row prt-table-row-odd" onclick>
+|      <div @key=key_600-0 data-row-id="key_600" class="prt-table-cell"> 62 </div>
+|      <div @key=key_600-1 data-row-id="key_600" class="prt-table-cell">
+|        <input oninput> </input>
+|        hi
+|      </div>
+|      <div @key=key_600-2 data-row-id="key_600" class="prt-table-cell"> 31.000000 </div>
      </div>
    </div>
  </div> |}]
;;

let%expect_test "typing into a column, leaving that column, and then coming back. " =
  let test =
    Test.create ~map:big_map ~visible_range:(50, 50) (Test.Component.default ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Handle.input_text
    test.handle
    ~get_vdom:Table.Result.view
    ~selector:".prt-table-cell:nth-child(2) input"
    ~text:"hello world";
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff test.handle;
  [%expect
    {|
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div> a </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
              <span> ◇  b </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    <div bounds-change=<fun>>
      <div @key=0 class="prt-table-row prt-table-row-even" onclick>
        <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 51 </div>
        <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
          <input oninput> </input>
-|        hi
+|        hi hello world
        </div>
        <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 25.000000 </div>
      </div>
      <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
        <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 52 </div>
        <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
          <input oninput> </input>
          hi
        </div>
        <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 26.000000 </div>
      </div>
    </div>
  </div> |}];
  (* move out of bounds (really 99-25 through 100) *)
  Test.set_bounds test ~low:99 ~high:99;
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  (* move back into bounds *)
  Test.set_bounds test ~low:50 ~high:50;
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
<div class="partial-render-table-bonsai_path_replaced_in_test">
  <table class="prt-table-header" size_tracker=<fun>>
    <tbody>
      <tr>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  key </span>
          </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div> a </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  b </span>
          </div>
        </td>
      </tr>
    </tbody>
  </table>
  <div bounds-change=<fun>>
    <div @key=0 class="prt-table-row prt-table-row-even" onclick>
      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 51 </div>
      <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
        <input oninput> </input>
        hi hello world
      </div>
      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 25.000000 </div>
    </div>
    <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 52 </div>
      <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
        <input oninput> </input>
        hi
      </div>
      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 26.000000 </div>
    </div>
  </div>
</div> |}]
;;

let%expect_test "table body is not recomputed more often than necessary" =
  (* The size_tracker and visibility hooks that PRT uses can be called by the browser more
     often than one would expect. For instance, if one places an element over the table,
     it causes the size_tracker hook on every column to fire. If you have a large table
     with lots of columns and lots of rows, it can be expensive to recompute the table
     body n times, once for each column. *)
  let test = Test.create (Test.Component.default ()) in
  Test.print_message_on_result_recomputation test;
  Test.resize_column test ~idx:0 ~width:1.;
  Test.set_bounds test ~low:0 ~high:300;
  Handle.flush test.handle;
  [%expect {|
    Initialized
    Changed
    Changed |}];
  (* Sanity check: re-stabilizing after doing no actions does not cause recomputation *)
  Handle.flush test.handle;
  [%expect {| |}];
  (* Re-setting a column to its existing width should not cause a re-fire *)
  Test.resize_column test ~idx:0 ~width:1.;
  Handle.flush test.handle;
  [%expect {| |}];
  (* Re-setting the bounds to the same value should not cause a re-fire *)
  Test.set_bounds test ~low:0 ~high:300;
  Handle.flush test.handle;
  [%expect {| |}]
;;

let%expect_test "table body is not recomputed more often than necessary" =
  let test =
    Test.create (fun input _filter_var ->
      let component =
        let%sub collation =
          Table_expert.collate
            ~filter_equal:[%compare.equal: unit]
            ~order_equal:[%compare.equal: unit]
            ~filter_to_predicate:(fun () -> None)
            ~order_to_compare:(fun () -> Unchanged)
            input
            (Value.return
               { Incr_map_collate.Collate.filter = ()
               ; order = ()
               ; key_range = All_rows
               ; rank_range = All_rows
               })
        in
        let columns =
          [ Table_expert.Columns.Dynamic_cells.column
              ~label:(Value.return (Vdom.Node.text "key"))
              ~cell:(fun ~key ~data:_ ->
                return
                @@ let%map key = key in
                Vdom.Node.textf "%d" key)
              ()
          ]
          |> Table_expert.Columns.Dynamic_cells.lift
        in
        Table_expert.component
          (module Int)
          ~focus:By_row
          ~row_height:(`Px 10)
          ~columns
          collation
      in
      { Test.Component.component
      ; get_vdom = Table_expert.Result.view
      ; get_testing = Table_expert.Result.for_testing
      ; get_inject = Shared.Test.Component.get_inject_expert
      })
  in
  Test.print_message_on_result_recomputation test;
  Test.resize_column test ~idx:0 ~width:1.;
  Test.set_bounds test ~low:0 ~high:300;
  Handle.flush test.handle;
  [%expect {|
    Initialized
    Changed
    Changed |}];
  (* Sanity check: re-stabilizing after doing no actions does not cause recomputation *)
  Handle.flush test.handle;
  [%expect {| |}];
  (* Changing the bounds should not cause a re-fire because we are doing our own collation
     and don't rely on result.bounds. *)
  Test.set_bounds test ~low:100 ~high:300;
  Handle.flush test.handle;
  [%expect {| Changed |}]
;;

let%expect_test "test is browser" =
  let open Js_of_ocaml in
  Dom_html.document |> Obj.magic |> Js_of_ocaml.Js.Optdef.test |> printf "%b";
  [%expect {| false |}]
;;

let%expect_test "sorting" =
  let test = Test.create (Test.Component.default ()) in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
<div class="partial-render-table-bonsai_path_replaced_in_test">
  <table class="prt-table-header" size_tracker=<fun>>
    <tbody>
      <tr>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  key </span>
          </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div> a </div>
        </td>
        <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
          <div onclick>
            <span> ◇  b </span>
          </div>
        </td>
      </tr>
    </tbody>
  </table>
  <div bounds-change=<fun>>
    <div @key=0 class="prt-table-row prt-table-row-even" onclick>
      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 0 </div>
      <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
        <input oninput> </input>
        hello
      </div>
      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 1.000000 </div>
    </div>
    <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 1 </div>
      <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
        <input oninput> </input>
        there
      </div>
      <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 2.000000 </div>
    </div>
    <div @key=200 class="prt-table-row prt-table-row-even" onclick>
      <div @key=key_200-0 data-row-id="key_200" class="prt-table-cell"> 4 </div>
      <div @key=key_200-1 data-row-id="key_200" class="prt-table-cell">
        <input oninput> </input>
        world
      </div>
      <div @key=key_200-2 data-row-id="key_200" class="prt-table-cell"> 2.000000 </div>
    </div>
  </div>
</div> |}];
  (* this one is the key, clicking on it does nothing (it's already sorted by the key) *)
  Handle.click_on test.handle ~selector:"td:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
  <div class="partial-render-table-bonsai_path_replaced_in_test">
    <table class="prt-table-header" size_tracker=<fun>>
      <tbody>
        <tr>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
-|            <span> ◇  key </span>
+|            <span> ⬘  key </span>
            </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div> a </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
              <span> ◇  b </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    <div bounds-change=<fun>>
      <div @key=0 class="prt-table-row prt-table-row-even" onclick>
        <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 0 </div> |}];
  (* this one actually does stuff, click on it twice for a reverse sort *)
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
  <div class="partial-render-table-bonsai_path_replaced_in_test">
    <table class="prt-table-header" size_tracker=<fun>>
      <tbody>
        <tr>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
-|            <span> ⬘  key </span>
+|            <span> ◇  key </span>
            </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div> a </div>
          </td>
          <td colspan="1" freeze_width=((set <fun>)(reset <fun>)) size_tracker=<fun>>
            <div onclick>
-|            <span> ◇  b </span>
+|            <span> ⬙  b </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    <div bounds-change=<fun>>
      <div @key=0 class="prt-table-row prt-table-row-even" onclick>
-|      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 0 </div>
+|      <div @key=key_0-0 data-row-id="key_0" class="prt-table-cell"> 1 </div>
        <div @key=key_0-1 data-row-id="key_0" class="prt-table-cell">
          <input oninput> </input>
-|        hello
+|        there
        </div>
-|      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 1.000000 </div>
+|      <div @key=key_0-2 data-row-id="key_0" class="prt-table-cell"> 2.000000 </div>
      </div>
      <div @key=100 class="prt-table-row prt-table-row-odd" onclick>
-|      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 1 </div>
+|      <div @key=key_100-0 data-row-id="key_100" class="prt-table-cell"> 4 </div>
        <div @key=key_100-1 data-row-id="key_100" class="prt-table-cell">
          <input oninput> </input>
-|        there
+|        world
        </div>
        <div @key=key_100-2 data-row-id="key_100" class="prt-table-cell"> 2.000000 </div>
      </div>
      <div @key=200 class="prt-table-row prt-table-row-even" onclick>
-|      <div @key=key_200-0 data-row-id="key_200" class="prt-table-cell"> 4 </div>
+|      <div @key=key_200-0 data-row-id="key_200" class="prt-table-cell"> 0 </div>
        <div @key=key_200-1 data-row-id="key_200" class="prt-table-cell">
          <input oninput> </input>
-|        world
+|        hello
        </div>
-|      <div @key=key_200-2 data-row-id="key_200" class="prt-table-cell"> 2.000000 </div>
+|      <div @key=key_200-2 data-row-id="key_200" class="prt-table-cell"> 1.000000 </div>
      </div>
    </div>
  </div> |}]
;;
