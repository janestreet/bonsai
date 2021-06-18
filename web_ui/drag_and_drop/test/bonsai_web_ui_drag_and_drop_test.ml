open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Node = Vdom.Node
module Attr = Vdom.Attr

let component =
  let universe name =
    Drag_and_drop.create
      [%here]
      ~source_id:(module Int)
      ~target_id:(module Int)
      ~on_drop:
        (Bonsai.Value.return (fun source target ->
           Ui_event.print_s
             [%message
               (sprintf "universe %s on_drop" name) (source : int) (target : int)]))
  in
  let%sub universe1 = universe "1" in
  let%sub universe2 = universe "2" in
  return
    (let%map source1 = universe1 >>| Drag_and_drop.source
     and target1 = universe1 >>| Drag_and_drop.drop_target
     and sentinel1 = universe1 >>| Drag_and_drop.sentinel
     and source2 = universe2 >>| Drag_and_drop.source
     and target2 = universe2 >>| Drag_and_drop.drop_target
     and sentinel2 = universe2 >>| Drag_and_drop.sentinel in
     let sentinel1 = sentinel1 ~name:"1" in
     let sentinel2 = sentinel2 ~name:"2" in
     Node.div
       [ Node.div
           ~attr:sentinel1
           [ Node.div ~attr:Attr.(id "s1" @ source1 ~id:1) []
           ; Node.div ~attr:Attr.(id "s23" @ source1 ~id:2 @ source2 ~id:3) []
           ; Node.div ~attr:Attr.(id "s4" @ source2 ~id:4) []
           ]
       ; Node.div
           ~attr:sentinel2
           [ Node.div ~attr:Attr.(id "t1" @ target1 ~id:1) []
           ; Node.div ~attr:Attr.(id "t23" @ target1 ~id:2 @ target2 ~id:3) []
           ; Node.div ~attr:Attr.(id "t4" @ target2 ~id:4) []
           ]
       ])
;;

let%expect_test "remove the component with the dnd" =
  let input_var = Bonsai.Var.create true in
  let component =
    if%sub Bonsai.Var.value input_var
    then component
    else Bonsai.const (Vdom.Node.text "no")
  in
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div data-dnd-name="1" dnd-test-hook=<fun>>
        <div id="s1" onmousedown={handler} style={ user-select: none; }> </div>
        <div id="s23" onmousedown={handler} style={ user-select: none; }> </div>
        <div id="s4" onmousedown={handler} style={ user-select: none; }> </div>
      </div>
      <div data-dnd-name="2" dnd-test-hook=<fun>>
        <div id="t1" data-drag-targetbonsai_path_replaced_in_test="1" onmouseup={handler}> </div>
        <div id="t23"
             data-drag-targetbonsai_path_replaced_in_test="2"
             data-drag-targetbonsai_path_replaced_in_test="3"
             onmouseup={handler}> </div>
        <div id="t4" data-drag-targetbonsai_path_replaced_in_test="4" onmouseup={handler}> </div>
      </div>
    </div>
    adding window event listener
    adding window event listener
    adding window event listener
    adding window event listener |}];
  Bonsai.Var.set input_var false;
  Handle.show handle;
  [%expect
    {|
    no
    removing window event listener
    removing window event listener
    removing window event listener
    removing window event listener |}]
;;

let%expect_test "how is it printed" =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div data-dnd-name="1" dnd-test-hook=<fun>>
        <div id="s1" onmousedown={handler} style={ user-select: none; }> </div>
        <div id="s23" onmousedown={handler} style={ user-select: none; }> </div>
        <div id="s4" onmousedown={handler} style={ user-select: none; }> </div>
      </div>
      <div data-dnd-name="2" dnd-test-hook=<fun>>
        <div id="t1" data-drag-targetbonsai_path_replaced_in_test="1" onmouseup={handler}> </div>
        <div id="t23"
             data-drag-targetbonsai_path_replaced_in_test="2"
             data-drag-targetbonsai_path_replaced_in_test="3"
             onmouseup={handler}> </div>
        <div id="t4" data-drag-targetbonsai_path_replaced_in_test="4" onmouseup={handler}> </div>
      </div>
    </div>
    adding window event listener
    adding window event listener
    adding window event listener
    adding window event listener |}]
;;

let run = Handle.Drag_and_drop.run ~get_vdom:Fn.id

let%expect_test "normal drag" =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  run handle ~name:"1" (Start_drag "1");
  run handle ~name:"1" (Set_target (Some "1"));
  run handle ~name:"1" Finish_drag;
  Handle.recompute_view handle;
  [%expect
    {|
    ("universe 1 on_drop" (source 1) (target 1))
    adding window event listener
    adding window event listener
    adding window event listener
    adding window event listener |}]
;;

let%expect_test "sources in both universes can go anywhere" =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  run handle ~name:"1" (Start_drag "2");
  run handle ~name:"1" (Set_target (Some "1"));
  run handle ~name:"1" Finish_drag;
  Handle.recompute_view handle;
  [%expect
    {|
    ("universe 1 on_drop" (source 2) (target 1))
    adding window event listener
    adding window event listener
    adding window event listener
    adding window event listener |}];
  run handle ~name:"2" (Start_drag "3");
  run handle ~name:"2" (Set_target (Some "3"));
  run handle ~name:"2" Finish_drag;
  Handle.recompute_view handle;
  [%expect {| ("universe 2 on_drop" (source 3) (target 3)) |}];
  run handle ~name:"2" (Start_drag "3");
  run handle ~name:"2" (Set_target (Some "4"));
  run handle ~name:"2" Finish_drag;
  Handle.recompute_view handle;
  [%expect {| ("universe 2 on_drop" (source 3) (target 4)) |}]
;;

let%expect_test "targets in both universes can receive anything" =
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  run handle ~name:"1" (Start_drag "1");
  run handle ~name:"1" (Set_target (Some "2"));
  run handle ~name:"1" Finish_drag;
  Handle.recompute_view handle;
  [%expect
    {|
    ("universe 1 on_drop" (source 1) (target 2))
    adding window event listener
    adding window event listener
    adding window event listener
    adding window event listener |}];
  run handle ~name:"1" (Start_drag "2");
  run handle ~name:"1" (Set_target (Some "2"));
  run handle ~name:"1" Finish_drag;
  Handle.recompute_view handle;
  [%expect {| ("universe 1 on_drop" (source 2) (target 2)) |}];
  run handle ~name:"2" (Start_drag "4");
  run handle ~name:"2" (Set_target (Some "3"));
  run handle ~name:"2" Finish_drag;
  Handle.recompute_view handle;
  [%expect {| ("universe 2 on_drop" (source 4) (target 3)) |}]
;;
