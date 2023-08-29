open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

let create ~title ~expect demo =
  let module Out = struct
    let name = title
    let description = expect
    let view = Bonsai.const demo
    let selector = None
    let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
  end
  in
  Gallery.make_demo (module Out)
;;

let component =
  let%sub theme, theme_picker = Gallery.Theme_picker.component ~default:Kado () in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Styled components - internal testing"
         , {|This example tests ppx_css inline syntax's browser interactions. Namely css specificity behavior correctly. |}
         , [ create
               ~title:"Variable definition in declaration"
               ~expect:{|Should be able to read it|}
               [%demo
                 Vdom.Node.div
                   ~attrs:
                     [ [%css
                       {| --color: tomato;
                          background-color: var(--color);
                          width: 2rem;
                          height: 2rem; |}]
                     ]
                   []]
           ; create
               ~title:
                 "Variable definition in declaration, but also set via an inline style"
               ~expect:{|hotpink should win|}
               [%demo
                 Vdom.Node.div
                   ~attrs:
                     [ [%css
                       {| --color: tomato;
                          background-color: var(--color);
                          width: 2rem;
                          height: 2rem; |}]
                     ; Vdom.Attr.css_var ~name:"color" "hotpink"
                     ]
                   []]
           ; create
               ~title:
                 "Variable definition in declaration, but also set via an inline style \
                  on a parent"
               ~expect:{|tomato should win|}
               [%demo
                 Vdom.Node.div
                   ~attrs:[ Vdom.Attr.css_var ~name:"color" "hotpink" ]
                   [ Vdom.Node.div
                       ~attrs:
                         [ [%css
                           {| --color: tomato;
                          background-color: var(--color);
                          width: 2rem;
                          height: 2rem; |}]
                         ]
                       []
                   ]]
           ; create
               ~title:"Attaching a class results in variable being set"
               ~expect:{|tomato is set|}
               [%demo
                 let color = `Name "tomato" in
                 let module Style =
                   [%css
                     stylesheet
                       {|
                    .square {
                       background-color: %{color#Css_gen.Color};
                       width: 2rem;
                       height: 2rem;
                     }
                       |}]
                 in
                 Vdom.Node.div ~attrs:[ Style.square ] []]
           ; create
               ~title:"Attaching an id results in variable being set"
               ~expect:{|tomato is set|}
               [%demo
                 let color = `Name "tomato" in
                 let module Style =
                   [%css
                     stylesheet
                       {|
                    #square {
                       background-color: %{color#Css_gen.Color};
                       width: 2rem;
                       height: 2rem;
                     }
                       |}]
                 in
                 Vdom.Node.div ~attrs:[ Style.square ] []]
           ; create
               ~title:"Attaching a variable results in a variable being set"
               ~expect:{|tomato is set on the bottom div, but not the top div|}
               [%demo
                 let color = `Name "tomato" in
                 let module Style =
                   [%css
                     stylesheet
                       {|
                    .foo * {
                       background-color: %{color#Css_gen.Color};
                       width: 2rem;
                       height: 2rem;
                       border: 1px solid black;
                       --beep: 1;
                     }
                       |}
                       ~dont_hash:[ "foo" ]]
                 in
                 Vdom.Node.div
                   ~attrs:[ Vdom.Attr.class_ "foo" ]
                   [ Vdom.Node.div []
                   ; Vdom.Node.div ~attrs:[ Style.Variables.set ~beep:"1" () ] []
                   ]]
           ] )
       ])
;;

let () =
  Async_js.init ();
  Auto_reload.refresh_on_build ();
  Bonsai_web.Start.start component
;;
