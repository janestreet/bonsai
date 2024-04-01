open! Core
open! Bonsai_web.Cont

module Style =
[%css
stylesheet
  {|
  .box {
    width:  100px;
    height: 100px;
    background-color: var(--my-color);
    border-radius: var(--radius);
    border: 3px solid black;
  }
|}
  ~rewrite:[ "--my-color", "--my-color"; "--radius", "--radius" ]]

let component =
  let red_box =
    Vdom.Node.div
      ~attrs:[ Style.box; Style.Variables.set ~my_color:"red" ~radius:"30px" () ]
      []
  in
  let blue_box =
    Vdom.Node.div ~attrs:[ Style.box; Style.Variables.set ~radius:"10px" () ] []
  in
  let other_box =
    Vdom.Node.div
      ~attrs:[ Style.box; Style.Variables.set_all ~radius:"4rem" ~my_color:"tomato" ]
      []
  in
  Vdom.Node.div
    ~attrs:[ Style.Variables.set ~my_color:"green" () ]
    [ red_box; blue_box; other_box ]
;;

let () =
  Bonsai_web.Start.start (fun _graph -> Bonsai.return (Vdom.Node.div [ component ]))
;;
