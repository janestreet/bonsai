open! Core
open! Bonsai_web

module Css =
  [%css.raw
    {|
  @font-face {
    font-family: "FiraCode";
    src: url(./font.ttf) format("truetype");
  }

  .firacode {
    font-family: "FiraCode";
  }
  |}]

let component =
  Bonsai.const
    (Vdom.Node.div
       ~attr:(Vdom.Attr.class_ Css.firacode)
       [ Vdom.Node.text "text with some ligatures -> ==> >>=" ])
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
