open! Core
open! Bonsai_web_test
open! Bonsai_web
open Bonsai_examples_mouse_position_lib

let%expect_test "basic page appearance" =
  let handle = Handle.create (Result_spec.vdom Fn.id) App.app in
  Handle.show handle;
  [%expect
    {|
    <div class="container_hash_replaced_in_test" onmousemove>
      <div class="sidebar_hash_replaced_in_test">
        <table class="table_hash_replaced_in_test"
               custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
          <thead class="header_hash_replaced_in_test">
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1" class="header_cell_hash_replaced_in_test"> Session </th>
              <th colspan="1" class="header_cell_hash_replaced_in_test"> Username </th>
              <th colspan="1" class="header_cell_hash_replaced_in_test"> Coordinates </th>
            </tr>
          </thead>
          <tbody class="body_hash_replaced_in_test"> </tbody>
        </table>
      </div>
    </div>
    |}]
;;
