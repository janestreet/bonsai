open! Core
module View = Bonsai_web_ui_view
module Vdom = Virtual_dom.Vdom
module Effect = Virtual_dom.Vdom.Effect
module Color = Css_gen.Color

include (
  View.Constants : module type of View.Constants with module Table := View.Constants.Table)
