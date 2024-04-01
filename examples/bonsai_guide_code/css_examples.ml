open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

(* $MDX part-begin=ppx_css_inline *)
let view =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
          background-color: tomato;
          min-width: 2rem;
          min-height: 2rem;
        |}]
      ]
    [ Vdom.Node.text "Very Red Background" ]
;;

(* $MDX part-end *)

let () = Util.run_vdom view ~id:"ppx_css_inline"

(* $MDX part-begin=ppx_css_inline_interpol *)
let box_with_border (color : Css_gen.Color.t) (width : Css_gen.Length.t) =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
          border: %{width#Css_gen.Length} solid %{color#Css_gen.Color};
        |}]
      ]
    [ Vdom.Node.text "Nice Borders!" ]
;;

(* $MDX part-end *)

let () =
  Util.run_vdom (box_with_border (`Name "purple") (`Px 2)) ~id:"ppx_css_inline_interpol"
;;

(* $MDX part-begin=ppx_css_inline_nesting *)
let hoverable_blocks =
  let block =
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
            background-color: green;
            min-width: 2rem;
            min-height: 2rem;
            border: 1px solid black;

            &:hover {
              background-color: tomato;
            }

            &:not(:nth-child(odd)):hover {
              background-color: purple;
            }
          |}]
        ]
      [ Vdom.Node.text "Hoverable" ]
  in
  Vdom.Node.div (List.init 6 ~f:(fun _ -> block))
;;

(* $MDX part-end *)

let () = Util.run_vdom hoverable_blocks ~id:"ppx_css_inline_nesting"

(* $MDX part-begin=ppx_css_inline_multiple *)
let multiple_ppx_css =
  Vdom.Node.div
    ~attrs:[ [%css {|color: red|}] ]
    [ Vdom.Node.text "Foo"
    ; Vdom.Node.div ~attrs:[ [%css "color: blue"] ] [ Vdom.Node.text "Bar" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run_vdom multiple_ppx_css ~id:"ppx_css_inline_multiple"

(* $MDX part-begin=ppx_css_stylesheet *)
module Style =
[%css
stylesheet
  {|
@media only screen and (max-width: 300px) {
  .container {
    display: none;
  }
}

@media only screen and (min-width: 300px) {
  .container {
    font-size: 10px;
  }
}

@media only screen and (min-width: 600px) {
  .container {
    font-size: 20px;
  }
}
    |}]

let stylesheet_demo = Vdom.Node.div ~attrs:[ Style.container ] [ Vdom.Node.text "Hello" ]

(* $MDX part-end *)

let () = Util.run_vdom stylesheet_demo ~id:"ppx_css_stylesheet"

(* $MDX part-begin=ppx_css_stylesheet_interpol *)

let stylesheet_interpol small_bg large_bg =
  let module Style =
  [%css
  stylesheet
    {|
@media only screen and (max-width: 1200px) {
  .container {
    background-color: %{small_bg#Css_gen.Color};
  }
}

@media only screen and (min-width: 1200px) {
  .container {
    background-color: %{large_bg#Css_gen.Color};
  }
}
  |}]
  in
  Vdom.Node.div ~attrs:[ Style.container ] [ Vdom.Node.text "Hello" ]
;;

(* $MDX part-end *)

let () =
  Util.run_vdom
    (stylesheet_interpol (`Name "purple") (`Name "green"))
    ~id:"ppx_css_stylesheet_interpol"
;;

module _ = struct
  (* $MDX part-begin=ppx_css_stylesheet_vars *)
  module Style =
  [%css
  stylesheet
    {|
@media only screen and (max-width: 1200px) {
  .container {
    background-color: var(--small-bg);
  }
}

@media only screen and (min-width: 1200px) {
  .container {
    background-color: var(--large-bg);
  }
}
|}]

  let stylesheet_vars =
    Vdom.Node.div
      ~attrs:
        [ Style.container; Style.Variables.set_all ~large_bg:"green" ~small_bg:"purple" ]
      [ Vdom.Node.text "Hello" ]
  ;;

  (* $MDX part-end *)

  let () = Util.run_vdom stylesheet_vars ~id:"ppx_css_stylesheet_vars"
end

(* $MDX part-begin=css_gen_inline *)
let css_gen_inline =
  let style : Css_gen.t =
    let open Css_gen in
    font_size (`Px 15) @> background_color (`Name "red")
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.style style ] [ Vdom.Node.text "Hello" ]
;;

(* $MDX part-end *)

let () = Util.run_vdom css_gen_inline ~id:"css_gen_inline"
