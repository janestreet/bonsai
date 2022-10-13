open! Core
open Bonsai_web

val to_vdom
  :  ?on_submit:Bonsai_web_ui_form.View.Private.submission_options
  -> ?editable:Bonsai_web_ui_form.View.Private.editable
  -> Bonsai_web_ui_form.View.t
  -> Vdom.Node.t
