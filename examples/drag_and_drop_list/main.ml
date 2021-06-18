open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Drag_and_drop = Bonsai_web_ui_drag_and_drop
module Reorderable_list = Bonsai_web_ui_reorderable_list

module S =
  [%css.raw
    {|
  .item {
    background-color: green;
    color: white;
    font-size:20px;
    padding: 5px;
    margin: 5px;
  }

  .transition_transform {
    transition: transform 0.3s, opacity 0.1s, background-color 0.3s;
  }
  |}]

let item ~now data =
  return
    (let%map now = now
     and data = data in
     let now = Time_ns.to_string_utc now in
     Vdom.Node.div
       ~attr:Vdom.Attr.(class_ S.item)
       [ Vdom.Node.text [%string "Item-%{data#Int} - now = %{now}"] ])
;;

let component =
  let input = List.init 30 ~f:(fun x -> x, x) |> Int.Map.of_alist_exn |> Value.return in
  let%sub now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) in
  let%sub data =
    Bonsai.assoc
      (module Int)
      input
      ~f:(fun key data ->
        let%sub item = item ~now data in
        return (Value.both item key))
  in
  let%sub dnd =
    Drag_and_drop.create
      [%here]
      ~source_id:(module Int)
      ~target_id:(module Int)
      ~on_drop:(Value.return (fun a b -> Ui_event.print_s [%message (a : int) (b : int)]))
  in
  let%sub sentinel = return (dnd >>| Drag_and_drop.sentinel) in
  let%sub dragged_element = Drag_and_drop.dragged_element dnd ~f:(item ~now) in
  let%sub list =
    Reorderable_list.list
      (module Int)
      ~dnd
      ~extra_item_attrs:(Value.return (Vdom.Attr.class_ S.transition_transform))
      ~item_height:40
      data
  in
  return
    (let%map sentinel = sentinel
     and dragged_element = dragged_element
     and list = list in
     Vdom.Node.div
       ~attr:(sentinel ~name:"list")
       [ Vdom.Node.text
           "This cannot actually be re-ordered, since it is a demo of the re-orderable \
            list library, which does not take responsibility for changing application \
            state."
       ; list
       ; dragged_element
       ])
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
