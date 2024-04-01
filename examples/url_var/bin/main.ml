open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

module Css =
[%css
stylesheet
  {|
.examples_column {
  display: flex;
  align-items: flex-start;
  flex-direction: row;
  flex-wrap: wrap;
}
|}]

let c s = s
let examples_column = c Css.examples_column

let component graph =
  let examples = Bonsai_web_ui_url_var_example_urls.examples graph in
  let%arr examples = examples in
  Vdom.Node.div [ Vdom.Node.div ~attrs:[ examples_column ] examples ]
;;

let () = Bonsai_web.Start.start component
