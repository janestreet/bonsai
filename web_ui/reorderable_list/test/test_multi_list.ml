open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Reorderable_list = Bonsai_web_ui_reorderable_list

let component render =
  let%sub lists, dragged_element =
    Reorderable_list.Multi.simple
      (module Int)
      (module Int)
      ~default_item_height:40
      ~render
      ~lists:(Value.return (Int.Set.of_list [ 0; 1; 2 ]))
      ~default_list:(Value.return 0)
      (Value.return (Int.Set.of_list [ 0; 1; 2; 3 ]))
  in
  let%sub lists =
    Bonsai.assoc
      (module Int)
      lists
      ~f:(fun which data ->
        let%sub _, view = return data in
        let%arr view = view
        and which = which in
        Vdom.Node.div
          [ Vdom.Node.h3 [ Vdom.Node.text [%string "List %{which#Int}"] ]; view ])
  in
  let%arr lists = lists
  and dragged_element = dragged_element in
  Vdom.Node.div [ Vdom.Node.div (Map.data lists); dragged_element ]
;;

let create_handle component =
  Handle.create
    (Result_spec.vdom
       ~path_censoring_message:""
       ~filter_printed_attributes:(fun ~key ~data:_ ->
         match key with
         | "style.transform" -> true
         | key when String.is_prefix key ~prefix:"data-drag-target" -> true
         | _ -> false)
       Fn.id)
    component
;;

let dnd_action handle action =
  Handle.Drag_and_drop.run handle ~get_vdom:Fn.id ~name:"dnd" action
;;

let%expect_test "simple usage" =
  let item ~index:_ ~source _which data =
    let%arr source = source
    and data = data in
    let view = Vdom.Node.div ~attrs:[ source ] [ Vdom.Node.text (Int.to_string data) ] in
    (), view
  in
  let handle = create_handle (component item) in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    adding window event listener
    adding window event listener
    |}];
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <div> 0 </div>
            </div>
            <div style={ transform: translateY(40px) translateX(0px); }>
              <div> 1 </div>
            </div>
            <div style={ transform: translateY(80px) translateX(0px); }>
              <div> 2 </div>
            </div>
            <div style={ transform: translateY(120px) translateX(0px); }>
              <div> 3 </div>
            </div>
          </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    |}];
  dnd_action handle (Start_drag "0");
  dnd_action handle (Set_target (Some "(1 0)"));
  dnd_action handle Finish_drag;
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <div> 1 </div>
            </div>
            <div style={ transform: translateY(40px) translateX(0px); }>
              <div> 2 </div>
            </div>
            <div style={ transform: translateY(80px) translateX(0px); }>
              <div> 3 </div>
            </div>
          </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <div> 0 </div>
            </div>
          </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    |}]
;;

let%expect_test "stateful items" =
  let item ~index:_ ~source _which data =
    let%sub is_true, toggle = Bonsai.toggle ~default_model:false in
    let%arr source = source
    and data = data
    and is_true = is_true
    and toggle = toggle in
    let view =
      Vdom.Node.button
        ~attrs:
          [ source
          ; Vdom.Attr.on_click (fun _ -> toggle)
          ; Vdom.Attr.id (Int.to_string data)
          ]
        [ Vdom.Node.textf "%d: %s" data (Bool.to_string is_true) ]
    in
    (), view
  in
  let handle = create_handle (component item) in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    adding window event listener
    adding window event listener
    |}];
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <button> 0: false </button>
            </div>
            <div style={ transform: translateY(40px) translateX(0px); }>
              <button> 1: false </button>
            </div>
            <div style={ transform: translateY(80px) translateX(0px); }>
              <button> 2: false </button>
            </div>
            <div style={ transform: translateY(120px) translateX(0px); }>
              <button> 3: false </button>
            </div>
          </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div> </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    |}];
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"#1";
  Handle.show_diff ~diff_context:0 handle;
  [%expect
    {|
    -|          <button> 1: false </button>
    +|          <button> 1: true </button>
    |}];
  dnd_action handle (Start_drag "1");
  dnd_action handle (Set_target (Some "(1 0)"));
  dnd_action handle Finish_drag;
  (* Observe that the state of the item is maintained after moving the item
     from one list to another one. *)
  Handle.show handle;
  [%expect
    {|
    <div>
      <div>
        <div>
          <h3> List 0 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <button> 0: false </button>
            </div>
            <div style={ transform: translateY(40px) translateX(0px); }>
              <button> 2: false </button>
            </div>
            <div style={ transform: translateY(80px) translateX(0px); }>
              <button> 3: false </button>
            </div>
          </div>
        </div>
        <div>
          <h3> List 1 </h3>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }>
              <button> 1: true </button>
            </div>
          </div>
        </div>
        <div>
          <h3> List 2 </h3>
          <div> </div>
        </div>
      </div>
      <div> </div>
    </div>
    |}]
;;
