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
    <div data-dnd-name="board" class="kanban-container" dnd-test-hook=<fun>>
      <div data-drag-targetbonsai_path_replaced_in_test="todo"
           class="kanban-column kanban-column-todo"
           onpointerup>
        <h3> Todo </h3>
        <div>
          <div @key=0 class="kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
          <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
        </div>
      </div>
      <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
           class="kanban-column kanban-column-in-progress"
           onpointerup>
        <h3> In Progress </h3>
        <div>
          <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
          <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
          <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
        </div>
      </div>
      <div data-drag-targetbonsai_path_replaced_in_test="finished"
           class="kanban-column kanban-column-finished"
           onpointerup>
        <h3> Done </h3>
        <div>
          <div @key=5 class="kanban-item" onpointerdown style={ user-select: none; }> finished 1 </div>
          <div @key=6 class="kanban-item" onpointerdown style={ user-select: none; }> finished 2 </div>
          <div @key=7 class="kanban-item" onpointerdown style={ user-select: none; }> finished 3 </div>
        </div>
      </div>
    </div>
    adding window event listener
    adding window event listener |}];
  run handle (Start_drag "0");
  run handle (Set_target (Some "finished"));
  run handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban-container" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="todo"
             class="kanban-column kanban-column-todo"
             onpointerup>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 class="kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
             class="kanban-column kanban-column-in-progress"
             onpointerup>
          <h3> In Progress </h3>
          <div>
            <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
            <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
            <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="finished"
             class="kanban-column kanban-column-finished"
             onpointerup>
          <h3> Done </h3>
          <div>
    +|      <div @key=0 class="kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=5 class="kanban-item" onpointerdown style={ user-select: none; }> finished 1 </div>
            <div @key=6 class="kanban-item" onpointerdown style={ user-select: none; }> finished 2 </div>
            <div @key=7 class="kanban-item" onpointerdown style={ user-select: none; }> finished 3 </div>
          </div>
        </div>
      </div> |}]
;;

let%expect_test "drag to same container" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener |}];
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
    adding window event listener |}];
  run handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban-container" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="todo"
             class="kanban-column kanban-column-todo"
             onpointerup>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 class="kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
    +|      <div @key=0 class="being-dragged kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
             class="kanban-column kanban-column-in-progress"
             onpointerup>
          <h3> In Progress </h3>
          <div>
            <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
            <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
            <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="finished"
             class="kanban-column kanban-column-finished"
             onpointerup>
          <h3> Done </h3>
          <div>
            <div @key=5 class="kanban-item" onpointerdown style={ user-select: none; }> finished 1 </div>
            <div @key=6 class="kanban-item" onpointerdown style={ user-select: none; }> finished 2 </div>
            <div @key=7 class="kanban-item" onpointerdown style={ user-select: none; }> finished 3 </div>
          </div>
    +|  </div>
    +|  <div style={
    +|         position: fixed;
    +|         top: 0px;
    +|         left: 0px;
    +|         pointer-events: none;
    +|         width: 0px;
    +|         height: 0px;
    +|         transform: translateY(0px) translateX(0px);
    +|       }>
    +|    <div class="kanban-item"> todo 1 </div>
        </div>
      </div> |}];
  run handle (Set_target (Some "todo"));
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban-container" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="todo"
    -|       class="kanban-column kanban-column-todo"
    +|       class="kanban-column kanban-column-active kanban-column-todo"
             onpointerup>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 class="being-dragged kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
    +|      <div @key=0 class="dragged-on-self kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
             class="kanban-column kanban-column-in-progress"
             onpointerup>
          <h3> In Progress </h3>
          <div>
            <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
            <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
            <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="finished"
             class="kanban-column kanban-column-finished"
             onpointerup> |}];
  run handle (Set_target (Some "finished"));
  Handle.show_diff handle;
  [%expect
    {|
      <div data-dnd-name="board" class="kanban-container" dnd-test-hook=<fun>>
        <div data-drag-targetbonsai_path_replaced_in_test="todo"
    -|       class="kanban-column kanban-column-active kanban-column-todo"
    +|       class="kanban-column kanban-column-todo"
             onpointerup>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 class="dragged-on-self kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
             class="kanban-column kanban-column-in-progress"
             onpointerup>
          <h3> In Progress </h3>
          <div>
            <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
            <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
            <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="finished"
    -|       class="kanban-column kanban-column-finished"
    +|       class="kanban-column kanban-column-active kanban-column-finished"
             onpointerup>
          <h3> Done </h3>
          <div>
    +|      <div @key=0 class="dragged-on-self kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=5 class="kanban-item" onpointerdown style={ user-select: none; }> finished 1 </div>
            <div @key=6 class="kanban-item" onpointerdown style={ user-select: none; }> finished 2 </div>
            <div @key=7 class="kanban-item" onpointerdown style={ user-select: none; }> finished 3 </div>
          </div>
        </div>
        <div style={
               position: fixed;
               top: 0px;
               left: 0px;
               pointer-events: none;
               width: 0px;
               height: 0px;
               transform: translateY(0px) translateX(0px);
             }>
          <div class="kanban-item"> todo 1 </div>
        </div> |}];
  run handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
          <h3> Todo </h3>
          <div>
            <div @key=1 class="kanban-item" onpointerdown style={ user-select: none; }> todo 2 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="in-progress"
             class="kanban-column kanban-column-in-progress"
             onpointerup>
          <h3> In Progress </h3>
          <div>
            <div @key=2 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 1 </div>
            <div @key=3 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 2 </div>
            <div @key=4 class="kanban-item" onpointerdown style={ user-select: none; }> in progress 3 </div>
          </div>
        </div>
        <div data-drag-targetbonsai_path_replaced_in_test="finished"
    -|       class="kanban-column kanban-column-active kanban-column-finished"
    +|       class="kanban-column kanban-column-finished"
             onpointerup>
          <h3> Done </h3>
          <div>
    -|      <div @key=0 class="dragged-on-self kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
    +|      <div @key=0 class="kanban-item" onpointerdown style={ user-select: none; }> todo 1 </div>
            <div @key=5 class="kanban-item" onpointerdown style={ user-select: none; }> finished 1 </div>
            <div @key=6 class="kanban-item" onpointerdown style={ user-select: none; }> finished 2 </div>
            <div @key=7 class="kanban-item" onpointerdown style={ user-select: none; }> finished 3 </div>
          </div>
    -|  </div>
    -|  <div style={
    -|         position: fixed;
    -|         top: 0px;
    -|         left: 0px;
    -|         pointer-events: none;
    -|         width: 0px;
    -|         height: 0px;
    -|         transform: translateY(0px) translateX(0px);
    -|       }>
    -|    <div class="kanban-item"> todo 1 </div>
        </div>
      </div> |}]
;;
