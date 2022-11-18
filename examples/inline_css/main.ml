open! Core
open! Bonsai_web

module Boxes = struct
  module Style =
    [%css
      stylesheet
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
|}
        (* [ppx_css] appends a hash to the end of each classname to allow you to be able
           to re-use classnames in multiple calls to [%css stylesheet] avoiding sadness
           for css identifier collisions like in this example where container is used
           multiple times.

           Sometimes, like when interacting with customization APIs that require specific
           classnames for CSS customization, hashing could get in your way, so you can override
           hashing behavior by using the optional "~rewrite" parameter.
        *)
        ~rewrite:[ "blue", "blue" ]]

  let component =
    Vdom.Node.div
      ~attr:Style.container
      [ Vdom.Node.div
          ~attr:(Vdom.Attr.many [ Style.box; Style.red (* "red_hash_#" *) ])
          []
      ; Vdom.Node.div
          ~attr:(Vdom.Attr.many [ Style.box; Style.green (* "green_hash_#" *) ])
          []
      ; Vdom.Node.div
          ~attr:(Vdom.Attr.many [ Style.box; Vdom.Attr.class_ "blue" (* "blue" *) ])
          []
      ]
  ;;
end

module Themeable = struct
  module Style =
    [%css
      stylesheet
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
|}
        (* Sometimes it might be useful to be able to use the same class-name
           defined from another call to [%css stylesheet] which you can do using the
           "~rewrite" optional flag.*)
        ~rewrite:[ "container", Boxes.Style.For_referencing.container ]]

  let component ?(style = Style.default) () =
    let module Style = (val style) in
    Vdom.Node.div
      ~attr:Style.container
      [ Vdom.Node.h1 [ Vdom.Node.text "this is a header" ]
      ; Vdom.Node.p [ Vdom.Node.text "this is some text" ]
      ]
  ;;
end

module My_theme =
  [%css
    stylesheet
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

  .container p { font-family: monospace; }
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
