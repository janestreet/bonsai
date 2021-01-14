open! Core_kernel
open! Bonsai_web_test
open! Bonsai_web
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

let component = Bonsai_drag_and_drop_example.app
let board = Bonsai_drag_and_drop_example.board

let%expect_test "drag between containers" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.show handle;
  [%expect
    {|
    <div class="kanban-container">
      <div class="kanban-column kanban-column-todo"
           ondragenter={handler}
           ondragleave={handler}
           ondragover={handler}
           ondrop={handler}>
        <h3> Todo </h3>
        <div>
          <div @key=0 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 1 </div>
          <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
        </div>
      </div>
      <div class="kanban-column kanban-column-in-progress"
           ondragenter={handler}
           ondragleave={handler}
           ondragover={handler}
           ondrop={handler}>
        <h3> In Progress </h3>
        <div>
          <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
          <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
          <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
        </div>
      </div>
      <div class="kanban-column kanban-column-finished"
           ondragenter={handler}
           ondragleave={handler}
           ondragover={handler}
           ondrop={handler}>
        <h3> Done </h3>
        <div>
          <div @key=5 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 1 </div>
          <div @key=6 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 2 </div>
          <div @key=7 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 3 </div>
        </div>
      </div>
    </div> |}];
  let (_ : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      Handle.Drag_and_drop.not_dragging
      [ Drag ".kanban-column-todo .kanban-item"; Enter ".kanban-column-finished"; Drop ]
  in
  Handle.show_diff handle;
  [%expect
    {|
    -1,37 +1,37
      <div class="kanban-container">
        <div class="kanban-column kanban-column-todo"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 1 </div>
            <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
          </div>
        </div>
        <div class="kanban-column kanban-column-in-progress"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> In Progress </h3>
          <div>
            <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
            <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
            <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
          </div>
        </div>
        <div class="kanban-column kanban-column-finished"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Done </h3>
          <div>
    +|      <div @key=0 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 1 </div>
            <div @key=5 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 1 </div>
            <div @key=6 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 2 </div>
            <div @key=7 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 3 </div>
          </div>
        </div>
      </div> |}]
;;

let%expect_test "drag to same container" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.store_view handle;
  [%expect {| |}];
  let (_ : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      Handle.Drag_and_drop.not_dragging
      [ Drag ".kanban-column-todo .kanban-item"; Enter ".kanban-column-todo"; Drop ]
  in
  (* No diff, since the item was dragged into the same column *)
  Handle.show_diff handle;
  [%expect {| |}]
;;

let%expect_test "appearance of dragged item and preview item while drag is happening" =
  let handle = Handle.create (Result_spec.vdom Fn.id) board in
  Handle.store_view handle;
  [%expect {| |}];
  let (dnd_state : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      Handle.Drag_and_drop.not_dragging
      [ Drag ".kanban-column-todo .kanban-item" ]
  in
  Handle.show_diff handle;
  [%expect
    {|
    -1,37 +1,41
      <div class="kanban-container">
    -|  <div class="kanban-column kanban-column-todo"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-todo"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 1 </div>
    +|      <div @key=0
    +|           draggable="true"
    +|           class="being-dragged kanban-item"
    +|           ondragend={handler}
    +|           ondragstart={handler}> todo 1 </div>
            <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
          </div>
        </div>
    -|  <div class="kanban-column kanban-column-in-progress"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-in-progress"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> In Progress </h3>
          <div>
            <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
            <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
            <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
          </div>
        </div>
    -|  <div class="kanban-column kanban-column-finished"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-finished"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Done </h3>
          <div>
            <div @key=5 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 1 </div>
            <div @key=6 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 2 </div>
            <div @key=7 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 3 </div>
          </div>
        </div>
      </div> |}];
  let (dnd_state : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      dnd_state
      [ Enter ".kanban-column-todo" ]
  in
  Handle.show_diff handle;
  [%expect
    {|
    -1,27 +1,27
      <div class="kanban-container">
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-todo"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-active kanban-column-todo"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Todo </h3>
          <div>
            <div @key=0
                 draggable="true"
    -|           class="being-dragged kanban-item"
    +|           class="dragged-on-self kanban-item"
                 ondragend={handler}
                 ondragstart={handler}> todo 1 </div>
            <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
          </div>
        </div>
        <div class="disable-child-pointer-events kanban-column kanban-column-in-progress"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> In Progress </h3>
          <div>
            <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
            <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
            <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
          </div> |}];
  let (dnd_state : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      dnd_state
      [ Leave; Enter ".kanban-column-finished" ]
  in
  Handle.show_diff handle;
  [%expect
    {|
    -1,41 +1,41
      <div class="kanban-container">
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-active kanban-column-todo"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-todo"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Todo </h3>
          <div>
    -|      <div @key=0
    -|           draggable="true"
    -|           class="dragged-on-self kanban-item"
    -|           ondragend={handler}
    -|           ondragstart={handler}> todo 1 </div>
            <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
          </div>
        </div>
        <div class="disable-child-pointer-events kanban-column kanban-column-in-progress"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> In Progress </h3>
          <div>
            <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
            <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
            <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
          </div>
        </div>
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-finished"
    +|  <div class="disable-child-pointer-events kanban-column kanban-column-active kanban-column-finished"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Done </h3>
          <div>
    +|      <div @key=0
    +|           draggable="true"
    +|           class="dragged-on-self kanban-item"
    +|           ondragend={handler}
    +|           ondragstart={handler}> todo 1 </div>
            <div @key=5 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 1 </div>
            <div @key=6 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 2 </div>
            <div @key=7 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 3 </div>
          </div>
        </div>
      </div> |}];
  let (_ : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run handle ~get_vdom:Fn.id dnd_state [ Drop ]
  in
  Handle.show_diff handle;
  [%expect
    {|
    -1,41 +1,37
      <div class="kanban-container">
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-todo"
    +|  <div class="kanban-column kanban-column-todo"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Todo </h3>
          <div>
            <div @key=1 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 2 </div>
          </div>
        </div>
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-in-progress"
    +|  <div class="kanban-column kanban-column-in-progress"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> In Progress </h3>
          <div>
            <div @key=2 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 1 </div>
            <div @key=3 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 2 </div>
            <div @key=4 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> in progress 3 </div>
          </div>
        </div>
    -|  <div class="disable-child-pointer-events kanban-column kanban-column-active kanban-column-finished"
    +|  <div class="kanban-column kanban-column-finished"
             ondragenter={handler}
             ondragleave={handler}
             ondragover={handler}
             ondrop={handler}>
          <h3> Done </h3>
          <div>
    -|      <div @key=0
    -|           draggable="true"
    -|           class="dragged-on-self kanban-item"
    -|           ondragend={handler}
    -|           ondragstart={handler}> todo 1 </div>
    +|      <div @key=0 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> todo 1 </div>
            <div @key=5 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 1 </div>
            <div @key=6 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 2 </div>
            <div @key=7 draggable="true" class="kanban-item" ondragend={handler} ondragstart={handler}> finished 3 </div>
          </div>
        </div>
      </div> |}]
;;

let%expect_test "drag to different universe does nothing" =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect
    {|
    <div>
      <p>
        You can drag items between each of the columns. When dropped, an item gets placed in a location determined by some ordering withing the column, rather than the position at which it is dropped. (This was more straightforward to implement). Items cannot be dragged between the two rows. (to demonstrate how multiple universes interact)
      </p>
      <div id="top-board">
        <div class="kanban-container">
          <div class="kanban-column kanban-column-todo"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> Todo </h3>
            <div>
              <div @key=0
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> todo 1 </div>
              <div @key=1
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> todo 2 </div>
            </div>
          </div>
          <div class="kanban-column kanban-column-in-progress"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> In Progress </h3>
            <div>
              <div @key=2
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 1 </div>
              <div @key=3
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 2 </div>
              <div @key=4
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 3 </div>
            </div>
          </div>
          <div class="kanban-column kanban-column-finished"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> Done </h3>
            <div>
              <div @key=5
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 1 </div>
              <div @key=6
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 2 </div>
              <div @key=7
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 3 </div>
            </div>
          </div>
        </div>
      </div>
      <div id="bottom-board">
        <div class="kanban-container">
          <div class="kanban-column kanban-column-todo"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> Todo </h3>
            <div>
              <div @key=0
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> todo 1 </div>
              <div @key=1
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> todo 2 </div>
            </div>
          </div>
          <div class="kanban-column kanban-column-in-progress"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> In Progress </h3>
            <div>
              <div @key=2
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 1 </div>
              <div @key=3
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 2 </div>
              <div @key=4
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> in progress 3 </div>
            </div>
          </div>
          <div class="kanban-column kanban-column-finished"
               ondragenter={handler}
               ondragleave={handler}
               ondragover={handler}
               ondrop={handler}>
            <h3> Done </h3>
            <div>
              <div @key=5
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 1 </div>
              <div @key=6
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 2 </div>
              <div @key=7
                   draggable="true"
                   class="kanban-item"
                   ondragend={handler}
                   ondragstart={handler}> finished 3 </div>
            </div>
          </div>
        </div>
      </div>
    </div> |}];
  let (_ : Handle.Drag_and_drop.t) =
    Handle.Drag_and_drop.run
      handle
      ~get_vdom:Fn.id
      Handle.Drag_and_drop.not_dragging
      [ Drag "#top-board .kanban-column-todo .kanban-item"
      ; Enter "#bottom-board .kanban-column-finished"
      ; Drop
      ]
  in
  (* No diff, since the item was dragged into the same column *)
  Handle.show_diff handle;
  [%expect {| |}]
;;
