open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Form = Bonsai_web_ui_form

module S =
[%css
stylesheet
  {|
  .item {
    background-color: green;
    color: white;
    font-size:20px;
    padding: 5px;
    margin: 5px;
  }

  .text_input {
    flex: 1;
  }

  .list {
    flex: 1;
  }

  .transition_transform {
    transition: transform 0.3s, opacity 0.1s, background-color 0.3s;
  }
  |}]

let item ~index:_ ~source _which _data =
  let%sub text, set_text = Bonsai.state_opt ~equal:[%equal: string] () in
  let%arr source = source
  and text = text
  and set_text = set_text in
  let view =
    View.hbox
      ~attrs:[ S.item; source ]
      [ Vdom_input_widgets.Entry.text
          ~extra_attrs:[ S.text_input ]
          ~value:text
          ~on_input:set_text
          ~allow_updates_when_focused:`Never
          ()
      ]
  in
  (), view
;;

let component =
  let%sub input, extend_input =
    Bonsai.state_machine0
      ()
      ~sexp_of_model:[%sexp_of: Int.Set.t]
      ~equal:[%equal: Int.Set.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:(Int.Set.of_list [ 0; 1; 2 ])
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model () ->
        Set.add model (Set.length model))
  in
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_sec 1.0)
      (let%map extend_input = extend_input in
       extend_input ())
  in
  let%sub num_lists =
    Form.Elements.Number.int ~default:1 ~step:1 ~allow_updates_when_focused:`Never ()
  in
  let%sub whiches =
    let%arr num_lists = num_lists in
    let length = Int.max 0 (Form.value_or_default num_lists ~default:1) in
    Int.Set.of_list (List.range 0 length)
  in
  let%sub lists, dragged_element =
    Reorderable_list.Multi.simple
      (module Int)
      (module Int)
      ~extra_item_attrs:(Value.return S.transition_transform)
      ~default_item_height:40
      ~render:item
      ~lists:whiches
      ~default_list:(Value.return 0)
      input
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
          ~attrs:[ S.list ]
          [ Vdom.Node.h3 [ Vdom.Node.text [%string "List %{which#Int}"] ]; view ])
  in
  let%arr lists = lists
  and dragged_element = dragged_element
  and num_lists = num_lists in
  Vdom.Node.div
    [ Form.view_as_vdom num_lists; View.hbox (Map.data lists); dragged_element ]
;;

let () = Bonsai_web.Start.start component
