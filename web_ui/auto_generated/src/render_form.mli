open! Core
open Bonsai_web

val to_vdom
  :  ?theme:View.Theme.t
  -> ?on_submit:Bonsai_web_ui_form.View.submission_options
  -> ?editable:Bonsai_web_ui_form.View.editable
  -> Bonsai_web_ui_form.View.t
  -> Vdom.Node.t
