open! Core
open! Bonsai_web_test
open! Bonsai_web
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec
module Example = Bonsai_drag_and_drop_example

let board = Example.board "board"
let run = Handle.Drag_and_drop.run ~get_vdom:Fn.id ~name:"board"

let%expect_test "drag between containers" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.show handle;
  [%expect
    {|
    <div data-dnd-name="board" class="kanban_container_hash_replaced_in_test" dnd-test-hook=<fun>>
      <div data-drag-targetbonsai_path_replaced_in_test="Todo"
           class="kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
           onpointerup>
        <h3 class="centered_hash_replaced_in_test"> Todo </h3>
        <div>
          <div @key=0
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> todo 1 </div>
          <div @key=1
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> todo 2 </div>
        </div>
      </div>
      <div data-drag-targetbonsai_path_replaced_in_test="In_progress"
           class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
           onpointerup>
        <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
        <div>
          <div @key=2
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> in progress 1 </div>
          <div @key=3
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> in progress 2 </div>
          <div @key=4
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> in progress 3 </div>
        </div>
      </div>
      <div data-drag-targetbonsai_path_replaced_in_test="Finished"
           class="kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
           onpointerup>
        <h3 class="centered_hash_replaced_in_test"> Done </h3>
        <div>
          <div @key=5
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> finished 1 </div>
          <div @key=6
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> finished 2 </div>
          <div @key=7
               class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
               onpointerdown> finished 3 </div>
        </div>
      </div>
    </div>
    adding window event listener
    adding window event listener
    |}];
  run handle (Start_drag "0");
  run handle (Set_target (Some "finished"));
  run handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban_container_hash_replaced_in_test" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="Todo"
             class="kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Todo </h3>
          <div>
    -|      <div @key=0
    -|           class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    -|           onpointerdown> todo 1 </div>
            <div @key=1
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="In_progress"
             class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
          <div>
            <div @key=2
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 1 </div>
            <div @key=3
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 2 </div>
            <div @key=4
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="Finished"
             class="kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Done </h3>
          <div>
    +|      <div @key=0
    +|           class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    +|           onpointerdown> todo 1 </div>
            <div @key=5
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 1 </div>
            <div @key=6
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 2 </div>
            <div @key=7
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 3 </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "drag to same container" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener
    |}];
  run handle (Start_drag "0");
  run handle (Set_target (Some "todo"));
  run handle Finish_drag;
  (* No diff, since the item was dragged into the same column *)
  Handle.show_diff handle;
  [%expect {| |}]
;;

let%expect_test "appearance of dragged item and preview item while drag is happening" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener
    |}];
  run handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban_container_hash_replaced_in_test" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="Todo"
             class="kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Todo </h3>
          <div>
            <div @key=0
    -|           class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    +|           class="being_dragged_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 1 </div>
            <div @key=1
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="In_progress"
             class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
          <div>
            <div @key=2
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 1 </div>
            <div @key=3
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"

        <div data-drag-targetbonsai_path_replaced_in_test="Finished"
             class="kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Done </h3>
          <div>
            <div @key=5
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 1 </div>
            <div @key=6
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 2 </div>
            <div @key=7
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 3 </div>
          </div>
        </div>
    +|  <div class="dragged_element_hash_replaced_in_test"
    +|       style={
    +|         width: 0px;
    +|         height: 0px;
    +|         transform: translateY(0px) translateX(0px);
    +|       }>
    +|    <div class="kanban_item_hash_replaced_in_test"> todo 1 </div>
    +|  </div>
      </div>
    |}];
  run handle (Set_target (Some "todo"));
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban_container_hash_replaced_in_test" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="Todo"
    -|       class="kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
    +|       class="kanban_column_active_hash_replaced_in_test kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Todo </h3>
          <div>
            <div @key=0
    -|           class="being_dragged_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    +|           class="dragged_on_self_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 1 </div>
            <div @key=1
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="In_progress"
             class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
          <div>
            <div @key=2
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 1 </div>
            <div @key=3
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    |}];
  run handle (Set_target (Some "finished"));
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban_container_hash_replaced_in_test" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="Todo"
    -|       class="kanban_column_active_hash_replaced_in_test kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
    +|       class="kanban_column_hash_replaced_in_test kanban_column_todo_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Todo </h3>
          <div>
    -|      <div @key=0
    -|           class="dragged_on_self_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    -|           onpointerdown> todo 1 </div>
            <div @key=1
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="In_progress"
             class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
          <div>
            <div @key=2
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 1 </div>
            <div @key=3
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 2 </div>
            <div @key=4
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="Finished"
    -|       class="kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
    +|       class="kanban_column_active_hash_replaced_in_test kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Done </h3>
          <div>
    +|      <div @key=0
    +|           class="dragged_on_self_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    +|           onpointerdown> todo 1 </div>
            <div @key=5
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 1 </div>
            <div @key=6
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 2 </div>
            <div @key=7
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 3 </div>
          </div>
        </div>
        <div class="dragged_element_hash_replaced_in_test"
             style={
               width: 0px;
               height: 0px;
               transform: translateY(0px) translateX(0px);
    |}];
  run handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
             class="kanban_column_hash_replaced_in_test kanban_column_in_progress_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> In Progress </h3>
          <div>
            <div @key=2
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 1 </div>
            <div @key=3
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 2 </div>
            <div @key=4
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="Finished"
    -|       class="kanban_column_active_hash_replaced_in_test kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
    +|       class="kanban_column_finished_hash_replaced_in_test kanban_column_hash_replaced_in_test"
             onpointerup>
          <h3 class="centered_hash_replaced_in_test"> Done </h3>
          <div>
            <div @key=0
    -|           class="dragged_on_self_hash_replaced_in_test kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
    +|           class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> todo 1 </div>
            <div @key=5
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 1 </div>
            <div @key=6
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 2 </div>
            <div @key=7
                 class="kanban_item_hash_replaced_in_test no_select_hash_replaced_in_test"
                 onpointerdown> finished 3 </div>
          </div>
        </div>
    -|  <div class="dragged_element_hash_replaced_in_test"
    -|       style={
    -|         width: 0px;
    -|         height: 0px;
    -|         transform: translateY(0px) translateX(0px);
    -|       }>
    -|    <div class="kanban_item_hash_replaced_in_test"> todo 1 </div>
    -|  </div>
      </div>
    |}]
;;
