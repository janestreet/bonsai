open! Core
open! Bonsai_web


module Style =
  [%css
    stylesheet
      {|
   .app { }
   .navbar { }
   .other_styles { }
   |}
      ~rewrite:[ "app", "app"; "navbar", Other_styles.navbar ]]

let component =
  Bonsai.const
    (Vdom.Node.span ~attr:(Vdom.Attr.class_ Style.app) [ Vdom.Node.text "hello world" ])
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
