open! Core
open! Bonsai_web

module Boxes = struct
  module Style =
    [%css.raw
      {|
  .container {
    display: inline-block;
  }

  .box {
    width:  100px;
    height: 100px;
  }

  .red  {
    background-color: red;
  }

  .green {
    background-color: green;
  }

  .blue {
    background-color: blue;
  }
|}]

  let component =
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Style.container)
      [ Vdom.Node.div ~attr:(Vdom.Attr.classes [ Style.box; Style.red ]) []
      ; Vdom.Node.div ~attr:(Vdom.Attr.classes [ Style.box; Style.green ]) []
      ; Vdom.Node.div ~attr:(Vdom.Attr.classes [ Style.box; Style.blue ]) []
      ]
  ;;
end

module Themeable = struct
  module Style =
    [%css.raw
      {|
    .container {
      padding: 1em;
      border: 1px solid black;
      display: inline-block;
    }

    .container h1 {
      font-size: 1.5em;
      color: rgb(20, 0, 0);
      margin: 0;
    }

    .container p {
      font-family: sans-serif;
    }
|}]

  let component ?(style = Style.default) () =
    let module Style = (val style) in
    Vdom.Node.div
      ~attr:(Vdom.Attr.class_ Style.container)
      [ Vdom.Node.h1 [ Vdom.Node.text "this is a header" ]
      ; Vdom.Node.p [ Vdom.Node.text "this is some text" ]
      ]
  ;;
end

module My_theme =
  [%css.raw
    {|
  .container {
    border: 1px solid red;
    display: inline-block;
    padding: 5px 5px 2px 2px;
  }

  .container h1 {
    font-weight: bold;
    color: rgb(10, 10, 10);
    margin: 0;
  }

  .container p {
    font-family: monospace;
  }
|}]

let (_ : _ Start.Handle.t) =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (Bonsai.const
       (Vdom.Node.div
          [ Vdom.Node.h1 [ Vdom.Node.text "Boxes" ]
          ; Boxes.component
          ; Vdom.Node.h1 [ Vdom.Node.text "Themeable Component" ]
          ; Themeable.component ()
          ; Themeable.component ~style:(module My_theme) ()
          ]))
;;
