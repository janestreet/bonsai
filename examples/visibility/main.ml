open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Vis = Bonsai_web_ui_visibility

module Style =
  [%css
    stylesheet
      {|

  body {
    font-family: sans-serif;
  }

  .box {
    width: 300px;
    height: 300px;

    border:1px solid black;
    display: flex;
    justify-content: center;
    align-items: center;
    border-radius:3px;
    font-size: 200px;
    color: white;
    -webkit-text-stroke: 1px black;

    transition: 0.5s all;
  }

  .visible {
    background: #e1f5fe;
    border-color: #03a9f4;
    -webkit-text-stroke: 1px #03a9f4;
  }

  .hidden {
    background: #f8bbd0;
    border-color: #e91e63;
    -webkit-text-stroke: 1px #e91e63;
  }

  .debug {
    position:fixed;
    top:0;
    background:white;
    left:400px;
  }
|}]

let visible_attr = Value.return (Vdom.Attr.class_ Style.visible)
let hidden_attr = Value.return (Vdom.Attr.class_ Style.hidden)
let data = List.init 30 ~f:(fun i -> i, ()) |> Int.Map.of_alist_exn |> Value.return

let view i =
  Vdom.Node.div
    ~attr:(Vdom.Attr.many [ Vdom.Attr.class_ Style.box ])
    [ Vdom.Node.textf "%d" i ]
;;

let component =
  let%sub components =
    Bonsai.assoc
      (module Int)
      data
      ~f:(fun key _data ->
        Vis.only_when_visible'
          ~visible_attr
          ~hidden_attr
          (let%arr key = key in
           view key, key))
  in
  let%arr components = components in
  let boxes, debug = components |> Map.data |> List.unzip in
  let boxes = View.vbox ~gap:(`Em 1) boxes in
  let debug =
    List.filter_opt debug
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:", "
    |> fun visible ->
    View.vbox
      ~attr:(Vdom.Attr.class_ Style.debug)
      [ Vdom.Node.h1 [ Vdom.Node.text ("Visible:" ^ visible) ] ]
  in
  Vdom.Node.div [ boxes; debug ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
