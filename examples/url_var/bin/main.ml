open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Css =
  [%css.raw
    {|
.examples_column {
  display: flex;
  align-items: flex-start;
  flex-direction: row;
  flex-wrap: wrap;
}
|}]

let c s = Vdom.Attr.class_ s
let examples_column = c Css.examples_column

let component =
  let%sub examples = Bonsai_web_ui_url_var_example_urls.examples in
  let%arr examples = examples in
  Vdom.Node.div [ Vdom.Node.div ~attr:examples_column examples ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
