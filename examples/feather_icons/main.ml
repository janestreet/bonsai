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
[%css
stylesheet
  {|
.class_ {
  display: flex;
  flex: 1 1 auto;
  flex-direction: column;
  gap: 48px;
}
|}]

let left_section ~controls graph =
  let%sub icons, search_bar = Search_bar.component graph in
  let%arr icons = icons
  and search_bar = search_bar
  and controls = controls in
  let icon_grid = Icon_grid.component ~icons ~controls in
  Vdom.Node.div ~attrs:[ Left_section.class_ ] [ search_bar; icon_grid ]
;;

module Main = [%css stylesheet {|
.class_ {
  display: flex;
  gap: 48px;
}
|}]

let main graph =
  let%sub controls, controls_view = Controls.component graph in
  let left_section = left_section ~controls graph in
  let%arr left_section = left_section
  and controls_view = controls_view in
  Vdom.Node.main ~attrs:[ Main.class_ ] [ left_section; controls_view ]
;;

let header =
  Vdom.Node.h2
    ~attrs:[ Vdom.Attr.style (Css_gen.font_weight (`Number 400)) ]
    [ Vdom.Node.text "Feather icons" ]
;;

module App = [%css stylesheet {|
.class_ {
  padding: 48px;
}
|}]

let app graph =
  let main = main graph in
  let%arr main = main in
  Vdom.Node.div ~attrs:[ App.class_ ] [ header; main ]
;;

let () = Bonsai_web.Start.start app
