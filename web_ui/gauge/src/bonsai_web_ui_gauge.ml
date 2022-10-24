open! Core
open Bonsai_web
module Svg = Virtual_dom_svg
open Vdom

module Styles =
  [%css
    stylesheet
      {|

.wrapper {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
}

|}]

let gauge ~radius ~(percent : Percent.t) ~percent_to_color =
  let percent_as_float = Percent.to_percentage percent in
  let stroke_width = radius *. 0.2 in
  let inner_radius = radius -. (stroke_width /. 2.) in
  let circumference = inner_radius *. 2.0 *. Float.pi in
  let arc = circumference *. (270. /. 360.) in
  let create_roundy_gauge_attr ~color ~extra_attr =
    Attr.many
      ([ Svg.Attr.cx radius
       ; Svg.Attr.cy radius
       ; Svg.Attr.fill (`Name "transparent")
       ; Svg.Attr.r inner_radius
       ; Svg.Attr.stroke color
       ; Svg.Attr.stroke_width stroke_width
       ; Svg.Attr.stroke_dasharray [ arc; circumference ]
       ; Svg.Attr.transform [ Rotate { a = `Deg 135.; x = radius; y = radius } ]
       ; Svg.Attr.stroke_linecap `Round
       ]
       @ extra_attr)
  in
  let base =
    Svg.Node.circle
      ~attr:(create_roundy_gauge_attr ~color:Tailwind_colors.gray200 ~extra_attr:[])
      []
  in
  let offset = arc -. (percent_as_float /. 100. *. arc) in
  let color =
    match percent_to_color with
    | Some f -> (f percent :> Css_gen.Color.t)
    | None -> (Tailwind_colors.blue500 :> Css_gen.Color.t)
  in
  let percent =
    Svg.Node.circle
      ~attr:
        (create_roundy_gauge_attr
           ~color
           ~extra_attr:[ Svg.Attr.stroke_dashoffset offset ])
      []
  in
  Svg.Node.svg
    ~attr:(Attr.many [ Svg.Attr.height (radius *. 2.); Svg.Attr.width (radius *. 2.) ])
    [ base; percent ]
;;

let create ?percent_to_color ~radius percent =
  let gauge = gauge ~radius ~percent ~percent_to_color in
  Node.div ~attr:(Attr.class_ Styles.wrapper) [ gauge ]
;;
