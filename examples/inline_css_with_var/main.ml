open! Core
open! Bonsai_web

module Style =
  [%css.raw
    {|
  .box {
    width:  100px;
    height: 100px;
    background-color: var(--my-color);
    border-radius: var(--radius);
    border: 3px solid black;
  }
|}]

let component =
  let red_box =
    Vdom.Node.div
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.class_ Style.box
           ; Vdom.Attr.css_var ~name:"radius" "30px"
           ; Vdom.Attr.css_var ~name:"my-color" "red"
           ])
      []
  in
  let blue_box =
    Vdom.Node.div
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.class_ Style.box; Vdom.Attr.css_var ~name:"radius" "10px" ])
      []
  in
  Vdom.Node.div ~attr:(Vdom.Attr.css_var ~name:"my-color" "green") [ red_box; blue_box ]
;;

let (_ : _ Start.Handle.t) =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (Bonsai.const (Vdom.Node.div [ component ]))
;;
