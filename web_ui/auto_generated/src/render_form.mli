open! Core
open Bonsai_web

val to_vdom
  :  ?theme:View.Theme.t
  -> ?on_submit:Bonsai_web_ui_form.With_automatic_view.View.submission_options
  -> ?editable:Bonsai_web_ui_form.With_automatic_view.View.editable
  -> Bonsai_web_ui_form.With_automatic_view.View.t
  -> Vdom.Node.t
