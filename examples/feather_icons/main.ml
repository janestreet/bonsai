open! Core
open! Import

(* global styles *)
let () =
  Inline_css.Private.append
    {|
*, *::before, *::after {
  box-sizing: border-box;
}

body {
  background-color: #f6f8fa;
  min-height: 100vh;
  width: 100%;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, "Noto Sans", sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji";
}

h3 {
  margin: 0;
}
|}
;;

module Left_section =
  [%css.raw
    {|
.class_ {
  display: flex;
  flex: 1 1 auto;
  flex-direction: column;
  gap: 48px;
}
|}]

let left_section ~controls =
  let%sub icons, search_bar = Search_bar.component in
  let%arr icons = icons
  and search_bar = search_bar
  and controls = controls in
  let icon_grid = Icon_grid.component ~icons ~controls in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ Left_section.class_) [ search_bar; icon_grid ]
;;

module Main = [%css.raw {|
.class_ {
  display: flex;
  gap: 48px;
}
|}]

let main =
  let%sub controls, controls_view = Controls.component in
  let%sub left_section = left_section ~controls in
  let%arr left_section = left_section
  and controls_view = controls_view in
  Vdom.Node.main ~attr:(Vdom.Attr.class_ Main.class_) [ left_section; controls_view ]
;;

let header =
  Vdom.Node.h2
    ~attr:(Vdom.Attr.style (Css_gen.font_weight (`Number 400)))
    [ Vdom.Node.text "Feather icons" ]
;;

module App = [%css.raw {|
.class_ {
  padding: 48px;
}
|}]


let app =
  let%sub main = main in
  let%arr main = main in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ App.class_) [ header; main ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app
;;
