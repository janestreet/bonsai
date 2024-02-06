open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Prt = Bonsai_web_ui_partial_render_table.Expert

module Vdom_tests (Columns : Shared_with_bench.S) = struct
  let%expect_test "Basic Vdom Test" =
    let component =
      let columns = Columns.all in
      let rows = Value.return (Shared_with_bench.Row.init_rows 5) in
      let%sub collate =
        let collate =
          Value.return
            { Incr_map_collate.Collate.filter = ()
            ; order = ()
            ; key_range = All_rows
            ; rank_range = All_rows
            }
        in
        Prt.collate
          ~filter_equal:[%compare.equal: unit]
          ~order_equal:[%compare.equal: unit]
          ~filter_to_predicate:(fun () -> None)
          ~order_to_compare:(fun () -> Unchanged)
          rows
          collate
      in
      Prt.component
        ~theming:`Themed
        ~focus:None
        (module Int)
        ~row_height:(Value.return (`Px 1))
        ~columns
        collate
    in
    let handle =
      Handle.create
        (module struct
          type t = (unit, Columns.column_id) Prt.Result.t

          let view { Prt.Result.view; _ } =
            Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn view
            |> Virtual_dom_test_helpers.Node_helpers.to_string_html
          ;;

          include Result_spec.No_incoming
        end)
        component
    in
    Handle.show handle;
    [%expect
      {|
      <div class="partial_render_table_container_hash_replaced_in_test table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-focused-fg_hash_replaced_in_test black)(--row-focused-border_hash_replaced_in_test #0a90bf)(--row-focused-bg_hash_replaced_in_test #e0f7ff)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--cell-focused-fg_hash_replaced_in_test black)(--cell-focused-bg_hash_replaced_in_test #e0f7ff)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <table class="header_hash_replaced_in_test partial_render_table_header_hash_replaced_in_test"
               bounds-change=<opaque>>
          <tbody>
            <tr class="header_row_hash_replaced_in_test">
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> symbol </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> max_edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bsize </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bid </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> ask </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> asize </td>
            </tr>
          </tbody>
        </table>
        <div class="partial-render-table-body-bonsai_path_replaced_in_test partial_render_table_body_hash_replaced_in_test"
             bounds-change=<opaque>
             style={
               height: 5px;
             }>
          <div class="body_hash_replaced_in_test" style={ padding-top: 0px; padding-bottom: 0px; }>
            <div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00000000px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> JANE0 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 0 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00000000px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> JANE1 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 1 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00000000px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> JANE2 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 2 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00000000px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> JANE3 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 3 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00000000px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> JANE4 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     onclick
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00000000px;
                       min-width: 0.00000000px;
                       max-width: 0.00000000px;
                     }> 4 </div>
              </div>
            </div>
          </div>
        </div>
      </div> |}]
  ;;
end

module _ = Vdom_tests (Shared_with_bench.Dynamic_cells)
module _ = Vdom_tests (Shared_with_bench.Dynamic_columns)
module _ = Vdom_tests (Shared_with_bench.Dynamic_experimental)
