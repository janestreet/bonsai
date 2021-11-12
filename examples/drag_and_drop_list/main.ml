open! Core
open! Bonsai_web
open Bonsai.Let_syntax
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

let item ~now ~index:_ ~source data =
  return
    (let%map now = now
     and source = source
     and data = data in
     let now = Time_ns.to_string_utc now in
     ( ()
     , Vdom.Node.div
         ~attr:Vdom.Attr.(class_ S.item @ source)
         [ Vdom.Node.text [%string "Item-%{data#Int} - now = %{now}"] ] ))
;;

let component =
  let%sub input, extend_input =
    Bonsai.state_machine0
      [%here]
      (module struct
        type t = Int.Set.t [@@deriving sexp, equal]
      end)
      (module Unit)
      ~default_model:(Int.Set.of_list [ 0; 1; 2 ])
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () ->
        Int.Set.add model (Int.Set.length model))
  in
  let%sub now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) in
  let%sub () =
    Bonsai.Clock.every
      [%here]
      (Time_ns.Span.of_sec 1.0)
      (let%map extend_input = extend_input in
       extend_input ())
  in
  let%sub _, view =
    Reorderable_list.simple
      (module Int)
      ~extra_item_attrs:(Value.return (Vdom.Attr.class_ S.transition_transform))
      ~default_item_height:40
      ~render:(item ~now)
      input
  in
  return view
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
