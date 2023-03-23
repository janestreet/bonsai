open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Node = Vdom.Node

let small_list = List.range 0 3 |> List.map ~f:(fun x -> x, x) |> Int.Map.of_alist_exn

let component list =
  let%sub dnd =
    Drag_and_drop.create
      ~source_id:(module Int)
      ~target_id:(module Int)
      ~on_drop:
        (Value.return (fun source target ->
           Ui_effect.print_s [%message "drag" (source : int) (target : int)]))
  in
  let%sub data =
    Bonsai.assoc
      (module Int)
      list
      ~f:(fun key data ->
        let%sub item =
          let%arr data = data in
          Node.text (Int.to_string data)
        in
        return (Value.both item key))
  in
  let%sub sentinel = return (dnd >>| Drag_and_drop.sentinel) in
  let%sub dragged_element =
    Drag_and_drop.dragged_element dnd ~f:(fun item ->
      let%arr item = item in
      Node.text (Int.to_string item))
  in
  let%sub source = return (dnd >>| Drag_and_drop.source) in
  let%sub list = Reorderable_list.list (module Int) ~dnd ~default_item_height:1 data in
  let%arr sentinel = sentinel
  and dragged_element = dragged_element
  and source = source
  and list = list in
  Node.div
    ~attrs:[ sentinel ~name:"dnd" ]
    [ Node.div ~attrs:[ source ~id:10 ] [ Node.text "10" ]; list; dragged_element ]
;;

let create_handle list =
  Handle.create
    (Result_spec.vdom
       ~path_censoring_message:""
       ~filter_printed_attributes:(fun key _data ->
         match key with
         | "style.transform" -> true
         | key when String.is_prefix key ~prefix:"data-drag-target" -> true
         | _ -> false)
       Fn.id)
    (component list)
;;

let%expect_test "how is it printed" =
  let handle = create_handle (Value.return small_list) in
  Handle.show handle;
  [%expect
    {|
    <div>
      <div> 10 </div>
      <div>
        <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
        <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
        <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
      </div>
    </div>
    adding window event listener
    adding window event listener |}]
;;

let dnd_action handle action =
  Handle.Drag_and_drop.run handle ~get_vdom:Fn.id ~name:"dnd" action
;;

let%expect_test "re-arrange" =
  let handle = create_handle (Value.return small_list) in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener |}];
  dnd_action handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    -|  </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
    +|  </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div> |}];
  dnd_action handle (Set_target (Some "2"));
  Handle.show_diff handle;
  [%expect {| |}];
  dnd_action handle (Set_target (Some "1"));
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 2 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
        <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div> |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
    (drag (source 0) (target 1))

      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
        </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div> |}]
;;

let%expect_test "drop onto empty list" =
  let handle = create_handle (Value.return Int.Map.empty) in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener |}];
  dnd_action handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
    -|  <div> </div>
    +|  <div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|  </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div> |}];
  dnd_action handle (Set_target (Some "10"));
  Handle.show_diff handle;
  [%expect {| |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  (* As expected, dropping an element on a non-existent target does not add the
     item to the list. *)
  [%expect
    {|
    (drag (source 0) (target 10))

      <div>
        <div> 10 </div>
    -|  <div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|  </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|  <div> </div>
      </div> |}]
;;

let%expect_test "add item from outside the list" =
  let handle = create_handle (Value.return small_list) in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener |}];
  dnd_action handle (Start_drag "10");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
          <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
      </div> |}];
  dnd_action handle (Set_target (Some "1"));
  Handle.show_diff handle;
  [%expect
    {|
        <div>
          <div> 10 </div>
          <div>
            <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      -|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
      +|    <div style={ transform: translateY(2px) translateX(0px); }> 1 </div>
      -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
      +|    <div style={ transform: translateY(3px) translateX(0px); }> 2 </div>
            <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
            <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
            <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
            <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
          </div>
          <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
        </div> |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
    (drag (source 10) (target 1))

      <div>
        <div> 10 </div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(3px) translateX(0px); }> 2 </div>
    -|    <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
        </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
      </div> |}]
;;

let%expect_test "re-arrange" =
  let handle = create_handle (Value.return small_list) in
  Handle.store_view handle;
  [%expect {|
    adding window event listener
    adding window event listener |}];
  Handle.Bulk_size_tracker.change_sizes
    handle
    ~get_vdom:Fn.id
    [ { selector = "div > div :nth-child(1)"; height = 2.0; width = 0. }
    ; { selector = "div > div :nth-child(2)"; height = 4.0; width = 0. }
    ];
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div style={ transform: translateY(6px) translateX(0px); }> 2 </div>
        </div>
      </div> |}]
;;

let component input =
  let%sub _, view =
    Reorderable_list.simple
      (module Int)
      ~sentinel_name:"dnd"
      ~default_item_height:1
      ~render:(fun ~index:_ ~source i ->
        let%arr i = i
        and source = source in
        (), Vdom.Node.div ~attrs:[ source ] [ Vdom.Node.text (Int.to_string i) ])
      input
  in
  return view
;;

let create_handle input =
  Handle.create
    (Result_spec.vdom
       ~filter_printed_attributes:(fun key _data ->
         match key with
         | "style.transform" -> true
         | _ -> false)
       Fn.id)
    (component input)
;;

let%expect_test "re-arrange" =
  let input = List.range 0 3 |> List.map ~f:Fn.id |> Int.Set.of_list |> Value.return in
  let handle = create_handle input in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div>
        <div style={ transform: translateY(0px) translateX(0px); }>
          <div> 0 </div>
        </div>
        <div style={ transform: translateY(1px) translateX(0px); }>
          <div> 1 </div>
        </div>
        <div style={ transform: translateY(2px) translateX(0px); }>
          <div> 2 </div>
        </div>
      </div>
    </div> |}];
  dnd_action handle (Start_drag "0");
  dnd_action handle (Set_target (Some "2"));
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }>
    +|      <div> 0 </div>
    +|    </div>
          <div style={ transform: translateY(0px) translateX(0px); }>
    -|      <div> 0 </div>
    +|      <div> 1 </div>
          </div>
          <div style={ transform: translateY(1px) translateX(0px); }>
    -|      <div> 1 </div>
    -|    </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }>
            <div> 2 </div>
          </div>
        </div>
      </div> |}]
;;

let%expect_test "dynamically extend list" =
  let input_var =
    List.range 0 3 |> List.map ~f:Fn.id |> Int.Set.of_list |> Bonsai.Var.create
  in
  let input = Bonsai.Var.value input_var in
  let handle = create_handle input in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div>
        <div style={ transform: translateY(0px) translateX(0px); }>
          <div> 0 </div>
        </div>
        <div style={ transform: translateY(1px) translateX(0px); }>
          <div> 1 </div>
        </div>
        <div style={ transform: translateY(2px) translateX(0px); }>
          <div> 2 </div>
        </div>
      </div>
    </div> |}];
  Bonsai.Var.update input_var ~f:(fun x -> Set.add x 20);
  Handle.recompute_view handle;
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }>
            <div> 0 </div>
          </div>
          <div style={ transform: translateY(1px) translateX(0px); }>
            <div> 1 </div>
          </div>
          <div style={ transform: translateY(2px) translateX(0px); }>
            <div> 2 </div>
          </div>
    +|    <div style={ transform: translateY(3px) translateX(0px); }>
    +|      <div> 20 </div>
    +|    </div>
        </div>
      </div> |}]
;;

type result =
  (int * int) list * Vdom.Node.t * (int Reorderable_list.Action.t -> unit Effect.t)

let%expect_test "removing an item should shift the rank of everything else" =
  let component =
    Reorderable_list.with_inject
      (module Int)
      ~sentinel_name:"dnd"
      ~default_item_height:1
      (fun ~index ~source:_ _ ->
         let%arr index = index in
         index, Vdom.Node.None)
  in
  let handle =
    Handle.create
      (module struct
        type t = result
        type incoming = int Reorderable_list.Action.t

        let incoming (_, _, inject) = inject
        let view (view, _, _) = Sexp.to_string_hum ([%sexp_of: (int * int) list] view)
      end : Result_spec.S
        with type t = result
         and type incoming = int Reorderable_list.Action.t)
      component
  in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect {|
    adding window event listener
    adding window event listener
    () |}];
  (* The result sexp shows the mapping from the item's key to its rank in the
     output order. Note that removing the 0 item makes the rank of the 1 item
     change from 1 to 0, as it should.

     In other words, removing an item from the ranking should cause any items
     below it to shift upward in the ranking by one. *)
  Handle.do_actions handle [ [ Set 0; Set 1 ] ];
  Handle.show handle;
  [%expect {|
    ((0 0) (1 1)) |}];
  Handle.do_actions handle [ [ Remove 0 ] ];
  Handle.show handle;
  [%expect {|
    ((1 0)) |}]
;;
