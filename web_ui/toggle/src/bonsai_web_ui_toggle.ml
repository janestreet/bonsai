open! Core
open Virtual_dom.Vdom

module Colors = struct
  type t =
    { toggle_text : Css_gen.Color.t
    ; inner_background : Css_gen.Color.t
    ; inner_border : Css_gen.Color.t
    ; inner_text : Css_gen.Color.t
    }

  let toggle t = Attr.style (Css_gen.color t.toggle_text)

  let inner t =
    Attr.style
      Css_gen.(
        border ~width:(`Px 1) ~color:t.inner_border ~style:`Solid ()
        @> color t.inner_text
        @> background_color t.inner_background)
  ;;
end

module Css =
[%css
stylesheet
  {|
    .container {
      position: relative;
    }

    .label {
      cursor: pointer;
    }

    .span {
      opacity: 50%;
      font-size: 1.2em;
    }

    .text {
      padding: 0.5em 1em 0.5em 1em;
      visibility: hidden;
      border-radius: 3px;
      position: absolute;
      z-index: 1;
      cursor: text;
      width: fit-content;
      min-width: 200px;
    }

    .right {
      left: 100%;
      top: -0.5em;
      transform: translateX(0.5em);
    }

    .left {
      right: 100%;
      top: -0.5em;
      transform: translateX(-0.5em);
    }

    .above {
      bottom: 100%;
      left: 50%;
      transform: translateX(-50%);
    }

    .below {
      top: 100%;
      left: 50%;
      transform: translateX(-50%);
    }

    .container:hover .text {
      visibility: visible;
    }

    .container:hover .span {
      opacity: 70%;
    }

    .checkbox:checked ~ .text {
      visibility: visible;
    }

    .checkbox:checked ~ .span {
      opacity: 100%;
    }

    .checkbox {
      position: absolute;
      opacity: 0%;
      cursor: pointer;
    }
    |}]

let view colors ~toggle ~inner ~direction =
  let direction_class =
    match direction with
    | `Above -> Css.above
    | `Below -> Css.below
    | `Left -> Css.left
    | `Right -> Css.right
  in
  Node.div
    ~attrs:[ Css.container ]
    [ Node.label
        ~attrs:[ Css.label; Colors.toggle colors ]
        [ Node.input ~attrs:[ Attr.type_ "checkbox"; Css.checkbox; Attr.tabindex (-1) ] ()
        ; Node.span ~attrs:[ Css.span ] [ toggle ]
        ; Node.div ~attrs:[ Css.text; Colors.inner colors; direction_class ] [ inner ]
        ]
    ]
;;
