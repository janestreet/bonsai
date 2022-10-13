open! Core
open Bonsai_web
open Bonsai_web_ui_element_size_hooks
open Bonsai_web_test
open Bonsai.Let_syntax

let%expect_test "position hook" =
  let computation =
    let%sub { Position_tracker.Position_tracker.positions; update = _; get_attr } =
      Position_tracker.component (module Int)
    in
    let%arr positions = positions
    and get_attr = get_attr in
    let mk id i =
      let attr = Vdom.Attr.many [ get_attr i; Vdom.Attr.id id ] in
      Vdom.Node.div ~attr []
    in
    Vdom.Node.div
      [ Vdom.Node.sexp_for_debugging
          [%message (positions : Position_tracker.Position.t Int.Map.t)]
      ; mk "a" 1
      ; mk "b" 2
      ; mk "c" 3
      ]
  in
  let handle = Handle.create (Result_spec.vdom Fn.id) computation in
  Handle.show handle;
  [%expect
    {|
    <div>
      <pre> (positions ()) </pre>
      <div id="a"
           bulk_position_tracker=((bulk_position_tracker_type_id(1)))
           style={
             box-sizing: border-box;
           }> </div>
      <div id="b"
           bulk_position_tracker=((bulk_position_tracker_type_id(2)))
           style={
             box-sizing: border-box;
           }> </div>
      <div id="c"
           bulk_position_tracker=((bulk_position_tracker_type_id(3)))
           style={
             box-sizing: border-box;
           }> </div>
    </div> |}];
  Handle.Position_tracker.change_positions
    handle
    ~get_vdom:Fn.id
    [ { selector = "#a"; height = 1; width = 1; top = 1; left = 1 }
    ; { selector = "#b"; height = 2; width = 2; top = 2; left = 2 }
    ; { selector = "#c"; height = 3; width = 3; top = 3; left = 3 }
    ];
  Handle.show handle;
  [%expect
    {|
    <div>
      <pre>
        (positions
     ((1 ((top 1) (left 1) (height 1) (width 1)))
      (2 ((top 2) (left 2) (height 2) (width 2)))
      (3 ((top 3) (left 3) (height 3) (width 3)))))
      </pre>
      <div id="a"
           bulk_position_tracker=((bulk_position_tracker_type_id(1)))
           style={
             box-sizing: border-box;
           }> </div>
      <div id="b"
           bulk_position_tracker=((bulk_position_tracker_type_id(2)))
           style={
             box-sizing: border-box;
           }> </div>
      <div id="c"
           bulk_position_tracker=((bulk_position_tracker_type_id(3)))
           style={
             box-sizing: border-box;
           }> </div>
    </div> |}]
;;
